module Application.GMM.MPPCA where

import           Application.GMM.MixtureModel
import           Application.GMM.PPCA
import           Control.Monad.IO.Class
import           CV.Utility.Parallel
import           Data.Binary
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.Matrix
import           Data.Maybe
import           Data.Time
import           Data.Vector                  as V
import           Prelude                      as P
import           System.Directory
import           System.Random
import           Text.Printf

type MPPCA = MixtureModel PPCA

type MPPCAData = Matrix Double

type MPPCAParameter = Matrix Double

initializeMPPCA
  :: PPCAInitParams -> Int -> Int -> IO MPPCA
initializeMPPCA initParams numModel numDimension =
  do time <- liftIO getCurrentTime
     let gen =
           mkStdGen . P.fromIntegral . diffTimeToPicoseconds . utctDayTime $
           time
         models' =
           V.unfoldrN numModel
                      (\g -> Just $ randomPPCA initParams numDimension g)
                      gen
     initializeMixture numModel models'

resetMPPCA
  :: PPCAInitParams -> MPPCA -> V.Vector Int -> IO MPPCA
resetMPPCA initParams model@(MixtureModel n modelVec) idx =
  do time <- liftIO getCurrentTime
     let gen =
           mkStdGen . P.fromIntegral . diffTimeToPicoseconds . utctDayTime $
           time
         (nD,nM) =
           (\(Model (_,gm)) -> (numDims gm,numZDims gm)) $ V.head modelVec
         models' =
           V.unfoldrN (V.length idx)
                      (\g -> Just $ randomPPCA initParams nD g)
                      gen
         idxModels = V.zip idx models'
     return $!
       MixtureModel
         n
         (V.generate
            n
            (\i ->
               let mi@(Model (wi,_)) = modelVec V.! i
               in case V.find (\(j,_) -> i == j) idxModels of
                    Nothing      -> mi
                    Just (_j,gm) -> Model (wi,gm)))

assignPoint
  :: Model PPCA -> Matrix Double -> Double -> MPPCAData -> Double
assignPoint (Model (w,m)) invM z x = w * (ppcaP' m invM x) / z

computeZS
  :: ParallelParams
  -> MPPCA
  -> V.Vector MPPCAData
  -> (V.Vector Double,V.Vector (Matrix Double))
computeZS parallelParams model@(MixtureModel n modelVec) xs
  | isJust zeroZIdx =
    error $
    "There is one data point which is assigned to none of the model. Try to increase the initialization range of sigma and to decrease that of mu.\n" P.++
    (show (xs V.! fromJust zeroZIdx))
  | otherwise = (zs,invM)
  where invM = computeInvMS model
        zs =
          parMapChunkVector
            parallelParams
            rdeepseq
            (\x ->
               V.foldl' (\s (im,(Model (wj,mj))) -> s + (wj * (ppcaP' mj im x))) 0 $
               V.zip invM modelVec)
            xs
        zeroZIdx = V.findIndex (== 0) zs

computeNKS
  :: ParallelParams
  -> PPCAInitParams
  -> MPPCA
  -> V.Vector (Matrix Double)
  -> V.Vector Double
  -> V.Vector MPPCAData
  -> IO (V.Vector Double,MPPCA,V.Vector (Matrix Double))
computeNKS parallelParams initParams model@(MixtureModel n modelVec) invM zs xs
  | V.length zeroKIdx > 0 =
    do putStrLn "There are models which have no point assigned to them! Reset them now."
       print zeroKIdx
       newModel <- resetMPPCA initParams model zeroKIdx
       computeNKS parallelParams
                  initParams
                  newModel
                  (computeInvMS newModel)
                  zs
                  xs
  | otherwise = return (nks,model,invM)
  where nks =
          parZipWithChunkVector
            parallelParams
            rdeepseq
            (\m im -> V.sum . V.zipWith (\z x -> assignPoint m im z x) zs $ xs)
            modelVec
            invM
        zeroKIdx =
          V.findIndices (\x -> x == 0 || isNaN x)
                        nks

computeInvMS :: MPPCA -> V.Vector (Matrix Double)
computeInvMS (MixtureModel _ modelVec) =
  V.map (\(Model (_,m)) -> computeInvM m) modelVec

getLikelihood :: V.Vector Double -> Double
getLikelihood = V.foldl' (\a b -> a + log b) 0

updatePiMPPCA
  :: Int -> V.Vector Double -> V.Vector Double
updatePiMPPCA n = V.map (/ fromIntegral n)

updateMuKMPPCA :: Model PPCA
               -> Matrix Double
               -> V.Vector Double
               -> V.Vector MPPCAData
               -> Double
               -> MPPCAParameter
updateMuKMPPCA mp invM zs xs nk =
  scaleMatrix (1 / nk) .
  V.foldl1' (elementwiseUnsafe (+)) .
  V.zipWith (\z x ->
               scaleMatrix (assignPoint mp invM z x)
                           x)
            zs $
  xs

updateMuMPPCA :: ParallelParams
              -> MPPCA
              -> V.Vector (Matrix Double)
              -> V.Vector Double
              -> V.Vector MPPCAData
              -> V.Vector Double
              -> V.Vector MPPCAParameter
updateMuMPPCA parallelParams model@(MixtureModel n modelVec) invM zs xs nks =
  parZipWith3ChunkVector parallelParams
                         rdeepseq
                         (\modelK nk im -> updateMuKMPPCA modelK im zs xs nk)
                         modelVec
                         nks
                         invM

updateWSigmaKMPPCA :: Model PPCA
                   -> Matrix Double
                   -> MPPCAParameter
                   -> V.Vector Double
                   -> V.Vector MPPCAData
                   -> (MPPCAParameter,Double)
updateWSigmaKMPPCA m@(Model (a,(PPCA nD nM wMOld _muM sigma'))) invM newMu zs xs =
  (newW,trace y2 / (P.fromIntegral nD))
  where n = P.fromIntegral $ V.length xs
        diagSigma = diagonal 0 (V.replicate nM sigma')
        s =
          scaleMatrix
            (1 / (n * a))
            (V.foldl1' (elementwiseUnsafe (+)) .
             V.zipWith (\z x ->
                          let r = assignPoint m invM z x
                              xmu = elementwiseUnsafe (-) x newMu
                          in scaleMatrix r
                                         (xmu * (transpose xmu)))
                       zs $
             xs)
        sw = s * wMOld
        x1 = invM * (transpose wMOld) * sw
        x2 = elementwiseUnsafe (+) x1 diagSigma
        x3 =
          case inverse x2 of
            Left msg -> error msg
            Right x' -> x'
        newW = sw * x3
        y1 = sw * invM * (transpose newW)
        y2 = elementwiseUnsafe (-) s y1

updateWSigmaMPPCA
  :: ParallelParams
  -> MPPCA
  -> V.Vector (Matrix Double)
  -> V.Vector MPPCAParameter
  -> V.Vector Double
  -> V.Vector MPPCAData
  -> (V.Vector MPPCAParameter,V.Vector Double)
updateWSigmaMPPCA parallelParams model@(MixtureModel n modelVec) invM newMu zs xs =
  V.unzip $
  parZipWith3ChunkVector parallelParams
                         rdeepseq
                         (\m im mu -> updateWSigmaKMPPCA m im mu zs xs)
                         modelVec
                         invM
                         newMu

em :: ParallelParams
   -> FilePath
   -> PPCAInitParams
   -> V.Vector MPPCAData
   -> Double
   -> Double
   -> MPPCA
   -> IO ()
em parallelParams filePath initParams xs threshold oldLikelihood oldModel =
  do let (zs,invM1) = computeZS parallelParams oldModel xs
     (nks,intermediateModel,invM) <-
       computeNKS parallelParams initParams oldModel invM1 zs xs
     let newLikelihood = getLikelihood zs
         (PPCA nD nM _ _ _) =
           snd . (\(Model x) -> x) . V.head . model $ intermediateModel
         avgLikelihood =
           log ((exp (newLikelihood / (P.fromIntegral $ V.length xs))) /
                (2 * pi) ** (-(P.fromIntegral nD) / 2))
         newPi =
           updatePiMPPCA (V.length xs)
                         nks
         newMu = updateMuMPPCA parallelParams intermediateModel invM zs xs nks
         (newW,newSigma) =
           updateWSigmaMPPCA parallelParams intermediateModel invM newMu zs xs
         newModel =
           MixtureModel (numModel intermediateModel) $
           V.zipWith4 (\a w mu s -> Model (a,PPCA nD nM w mu s))
                      newPi
                      newW
                      newMu
                      newSigma
     time <- liftIO getZonedTime
     let timeStr =
           (show . localTimeOfDay . zonedTimeToLocalTime $ time) P.++ ": "
     printf (timeStr P.++ "%0.2f (%0.3f%%)\n")
            avgLikelihood
            ((avgLikelihood - oldLikelihood) / (abs oldLikelihood) * 100)
     if avgLikelihood > threshold
        then liftIO $ encodeFile filePath intermediateModel
        else do liftIO $ encodeFile filePath intermediateModel
                em parallelParams filePath initParams xs threshold avgLikelihood newModel

mppcaSink :: ParallelParams
          -> PPCAInitParams
          -> Int
          -> Double
          -> FilePath
          -> Sink (V.Vector MPPCAData) IO ()
mppcaSink parallelParams initParams numModel threshold filePath =
  do xs <- consume
     fileFlag <- liftIO $ doesFileExist filePath
     models <-
       liftIO $
       if fileFlag
          then do fileSize <- liftIO $ getFileSize filePath
                  if fileSize > 0
                     then decodeFile filePath
                     else initializeMPPCA initParams
                                          numModel
                                          (nrows . V.head . P.head $ xs)
          else initializeMPPCA initParams
                               numModel
                               (nrows . V.head . P.head $ xs)
     let ys = V.concat xs
     liftIO $ em parallelParams filePath initParams ys threshold 0 models
