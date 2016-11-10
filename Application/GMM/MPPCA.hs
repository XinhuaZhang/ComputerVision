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


resetMPPCA :: PPCAInitParams -> MPPCA -> V.Vector Int -> IO MPPCA
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
  :: Model PPCA -> Double -> MPPCAData -> Double
assignPoint (Model (w,m)) z x = w * (ppcaP m x) / z

computeZS
  :: ParallelParams -> MPPCA -> V.Vector MPPCAData -> V.Vector Double
computeZS parallelParams model@(MixtureModel n modelVec) xs
  | isJust zeroZIdx =
    error $
    "There is one data point which is assigned to none of the model. Try to increase the initialization range of sigma and to decrease that of mu.\n" P.++
    (show (xs V.! fromJust zeroZIdx))
  | otherwise = zs
  where zs =
          parMapChunkVector
            parallelParams
            rdeepseq
            (\x ->
               V.foldl' (\s (Model (wj,mj)) -> s + (wj * (ppcaP mj x))) 0 modelVec)
            xs
        zeroZIdx = V.findIndex (== 0) zs

computeNKS :: ParallelParams
           -> PPCAInitParams
           -> MPPCA
           -> V.Vector Double
           -> V.Vector MPPCAData
           -> IO (V.Vector Double,MPPCA)
computeNKS parallelParams initParams model@(MixtureModel n modelVec) zs xs
  | V.length zeroKIdx > 0 =
    do putStrLn "There are models which have no point assigned to them! Reset them now."
       print zeroKIdx
       newModel <- resetMPPCA initParams model zeroKIdx
       computeNKS parallelParams initParams newModel zs xs
  | otherwise = return (nks,model)
  where nks =
          parMapChunkVector
            parallelParams
            rdeepseq
            (\m -> V.sum . V.zipWith (\z x -> assignPoint m z x) zs $ xs)
            modelVec
        zeroKIdx =
          V.findIndices (\x -> x == 0 || isNaN x)
                        nks

computeSS :: ParallelParams
          -> MPPCA
          -> V.Vector MPPCAParameter
          -> V.Vector Double
          -> V.Vector MPPCAData
          -> V.Vector MPPCAData
computeSS parallelParams model@(MixtureModel n modelVec) newMu zs xs = ss
  where n = P.fromIntegral $ V.length xs
        ss =
          parZipWithChunkVector
            parallelParams
            rdeepseq
            (\m@(Model (w,p)) newMu' ->
               scaleMatrix
                 (1 / (n * w))
                 (V.foldl1' (elementwiseUnsafe (+)) .
                  V.zipWith (\z x ->
                               let r = assignPoint m z x
                                   xmu = elementwiseUnsafe (-) x newMu'
                               in scaleMatrix r
                                              (xmu `multStd2` (transpose xmu)))
                            zs $
                  xs))
            modelVec
            newMu


getLikelihood :: V.Vector Double -> Double
getLikelihood = V.foldl' (\a b -> a + log b) 0

updatePiMPPCA :: Int -> V.Vector Double -> V.Vector Double
updatePiMPPCA n = V.map (/ fromIntegral n)

updateMuKMPPCA :: Model PPCA
               -> V.Vector Double
               -> V.Vector MPPCAData
               -> Double
               -> MPPCAParameter
updateMuKMPPCA mp zs xs nk =
  scaleMatrix (1 / nk) .
  V.foldl1' (elementwiseUnsafe (+)) .
  V.zipWith (\z x ->
               scaleMatrix (assignPoint mp z x)
                           x)
            zs $
  xs

updateMuMPPCA :: ParallelParams
              -> MPPCA
              -> V.Vector Double
              -> V.Vector MPPCAData
              -> V.Vector Double
              -> V.Vector MPPCAParameter
updateMuMPPCA parallelParams model@(MixtureModel n modelVec) zs xs nks =
  parZipWithChunkVector parallelParams
                        rdeepseq
                        (\modelK nk -> updateMuKMPPCA modelK zs xs nk)
                        modelVec
                        nks

updateWKMPPCA :: Model PPCA
              -> MPPCAData
              -> MPPCAParameter
updateWKMPPCA (Model (a,(PPCA _nD nM wM _muM sigma'))) s =
  s `multStd2` wM `multStd2` z
  where wt = transpose wM
        diagSigma = diagonal 0 (V.replicate nM sigma')
        m =
          elementwiseUnsafe (+)
                            diagSigma
                            (wt `multStd2` wM)
        invM =
          case inverse m of
            Left msg -> error msg
            Right y' -> y'
        x = invM `multStd2` wt `multStd2` s `multStd2` wM
        y = elementwiseUnsafe (+) x diagSigma
        z =
          case inverse y of
            Left msg -> error msg
            Right y' -> y'

updateWMPPCA :: ParallelParams
             -> MPPCA
             -> V.Vector MPPCAData
             -> V.Vector MPPCAParameter
updateWMPPCA parallelParams model@(MixtureModel n modelVec) ss =
  parZipWithChunkVector parallelParams
                        rdeepseq
                        (\m s -> updateWKMPPCA m s)
                        modelVec
                        ss

updateSigmaKMPPCA
  :: Model PPCA -> MPPCAParameter -> MPPCAData -> Double
updateSigmaKMPPCA (Model (a,(PPCA nD nM wMOld _muM sigma'))) wMNew s =
  (trace y) / (P.fromIntegral nD)
  where wt = transpose wMNew
        diagSigma = diagonal 0 (V.replicate nM sigma')
        m =
          elementwiseUnsafe (+)
                            diagSigma
                            (wt `multStd2` wMOld)
        invM =
          case inverse m of
            Left msg -> error msg
            Right y' -> y'
        x = s `multStd2` wMOld `multStd2` invM `multStd2` wt
        y = elementwiseUnsafe (-) s x

updateSigmaMPPCA :: ParallelParams
                 -> MPPCA
                 -> V.Vector MPPCAParameter
                 -> V.Vector MPPCAData
                 -> V.Vector Double
updateSigmaMPPCA parallelParams model@(MixtureModel n modelVec) newWs ss =
  parZipWith3ChunkVector parallelParams
                         rdeepseq
                         (\m w s -> updateSigmaKMPPCA m w s)
                         modelVec
                         newWs
                         ss

em :: ParallelParams
   -> FilePath
   -> PPCAInitParams
   -> V.Vector MPPCAData
   -> Double
   -> Double
   -> MPPCA
   -> IO ()
em parallelParams filePath initParams xs threshold oldLikelihood oldModel =
  do let zs = computeZS parallelParams oldModel xs
     (nks,intermediateModel) <-
       computeNKS parallelParams initParams oldModel zs xs
     let newLikelihood = getLikelihood zs
         (PPCA nD nM _ _ _) =
           snd . (\(Model x) -> x) . V.head . model $ intermediateModel
         avgLikelihood =
           log ((exp (newLikelihood / (P.fromIntegral $ V.length xs))) /
                (2 * pi) ** (-(P.fromIntegral nD) / 2))
         newPi =
           updatePiMPPCA (V.length xs)
                         nks
         newMu = updateMuMPPCA parallelParams intermediateModel zs xs nks
         ss = computeSS parallelParams intermediateModel newMu zs xs
         newW = updateWMPPCA parallelParams intermediateModel ss
         newSigma = updateSigmaMPPCA parallelParams intermediateModel newW ss
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

mppcaSink :: ParallelParams -> PPCAInitParams
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
