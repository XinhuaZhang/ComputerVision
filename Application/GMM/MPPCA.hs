module Application.GMM.MPPCA where

import           Application.GMM.Matrix
import           Application.GMM.MixtureModel
import           Application.GMM.PPCA
import           Control.Monad
import           Control.Monad.IO.Class
import           CV.Utility.Parallel
import           Data.Binary
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.Matrix
import           Data.Maybe
import           Data.Time
import           Data.Vector                  as V
import           Data.Vector.Unboxed          as VU
import           Prelude                      as P
import           System.Directory
import           System.Random
import           Text.Printf

type MPPCA = MixtureModel PPCA

type MPPCAData = VU.Vector Double

type MPPCAParameter = Matrix Double

initializeMPPCA :: PPCAInitParams -> Int -> Int -> IO MPPCA
initializeMPPCA initParams numModel' numDimension = do
  putStrLn "initializeMPPCA"
  time <- getCurrentTime
  let gen = mkStdGen . P.fromIntegral . diffTimeToPicoseconds . utctDayTime $ time
      models' =
        V.unfoldrN
          numModel'
          (Just . randomPPCA initParams numDimension)
          gen
  initializeMixture numModel' models'

resetMPPCA :: PPCAInitParams -> MPPCA -> V.Vector Int -> IO MPPCA
resetMPPCA initParams _model@(MixtureModel n modelVec) idx = do
  time <- getCurrentTime
  let gen = mkStdGen . P.fromIntegral . diffTimeToPicoseconds . utctDayTime $ time
      (nD, _nM) = (\(Model (_, gm)) -> (numDims gm, numZDims gm)) $ V.head modelVec
      models' =
        V.unfoldrN (V.length idx) (Just . randomPPCA initParams nD) gen
      idxModels = V.zip idx models'
  return $!
    MixtureModel
      n
      (V.generate
         n
         (\i ->
             let mi@(Model (wi, _)) = modelVec V.! i
             in case V.find (\(j, _) -> i == j) idxModels of
                  Nothing       -> mi
                  Just (_j, gm) -> Model (wi, gm)))

assignPoint :: Model PPCA -> Matrix Double -> Double -> MPPCAData -> Double
assignPoint (Model (w, m)) invM z x = w * ppcaP' m invM x / z

assignPointVec
  :: Model PPCA
  -> Matrix Double
  -> V.Vector Double
  -> V.Vector MPPCAData
  -> V.Vector Double
assignPointVec (Model (w, m)) invM zs xs =
  V.zipWith (\z x -> w * x / z) zs $ ppcaPVec' m invM xs

computeZS
  :: ParallelParams
  -> MPPCA
  -> V.Vector MPPCAData
  -> (V.Vector Double, V.Vector (Matrix Double))
computeZS parallelParams model'@(MixtureModel _n modelVec) xs
  | isJust zeroZIdx =
    error $
    "There is one data point which is assigned to none of the model. Try to increase the initialization range of sigma and to decrease that of mu.\n" P.++
    show (xs V.! fromJust zeroZIdx)
  | isJust nanZIdx =
    error $
    "Nan found! The variance of One of the models is too smalll." P.++
    show (xs V.! fromJust nanZIdx)
  | otherwise = (zs,invM)
  where invM = computeInvMS model'
        ys =
          parZipWithChunkVector
            parallelParams
            rdeepseq
            (\im (Model (wj,mj)) ->
               VU.map (* wj) . V.convert $ ppcaPVec' mj im xs)
            invM
            modelVec
        zs = VU.convert $ V.foldl1' (VU.zipWith (+)) ys
        zeroZIdx = V.findIndex (== 0) zs
        nanZIdx = V.findIndex isNaN zs

computeNKS
  :: ParallelParams
  -> PPCAInitParams
  -> MPPCA
  -> V.Vector MPPCAData
  -> IO (V.Vector Double, V.Vector Double, MPPCA, V.Vector (Matrix Double))
computeNKS parallelParams initParams model'@(MixtureModel _n modelVec) xs
  | V.length zeroKIdx > 0 =
    do putStrLn "There are models which have no point assigned to them! Reset them now."
       print zeroKIdx
       newModel <- resetMPPCA initParams model' zeroKIdx
       computeNKS parallelParams initParams newModel xs
  | V.length nanKIdx > 0 =
    do putStrLn "Found NaN! Reset them now."
       print nanKIdx
       newModel <- resetMPPCA initParams model' nanKIdx
       computeNKS parallelParams initParams newModel xs
  | otherwise = return (zs,nks,model',invM)
  where nks =
          parZipWithChunkVector
            parallelParams
            rdeepseq
            (\m im -> VU.sum . V.convert $ assignPointVec m im zs xs)
            modelVec
            invM
        (zs,invM) = computeZS parallelParams model' xs
        zeroKIdx =
          V.findIndices (== 0)
                        nks
        nanKIdx =
          V.findIndices isNaN
                        nks

computeInvMS :: MPPCA -> V.Vector (Matrix Double)
computeInvMS (MixtureModel _ modelVec) =
  V.map (\(Model (_, m)) -> computeInvM m) modelVec

getLikelihood :: V.Vector Double -> Double
getLikelihood = V.foldl' (\a b -> a + log b) 0

updatePiMPPCA :: Int -> V.Vector Double -> V.Vector Double
updatePiMPPCA n = V.map (/ fromIntegral n)

updateMuKMPPCA
  :: Model PPCA
  -> Matrix Double
  -> V.Vector Double
  -> V.Vector MPPCAData
  -> Double
  -> VU.Vector Double
updateMuKMPPCA mp invM zs xs nk =
  VU.map (/ nk) .
  V.foldl1' (VU.zipWith (+)) . V.zipWith (\y x -> VU.map (* y) x) ys $
  xs
  where
    ys = assignPointVec mp invM zs xs

updateMuMPPCA
  :: ParallelParams
  -> MPPCA
  -> V.Vector (Matrix Double)
  -> V.Vector Double
  -> V.Vector MPPCAData
  -> V.Vector Double
  -> V.Vector (VU.Vector Double)
updateMuMPPCA parallelParams _model@(MixtureModel _n modelVec) invM zs xs nks =
  parZipWith3ChunkVector
    parallelParams
    rdeepseq
    (\modelK nk im -> updateMuKMPPCA modelK im zs xs nk)
    modelVec
    nks
    invM

updateWSigmaKMPPCA
  :: Model PPCA
  -> Matrix Double
  -> VU.Vector Double
  -> V.Vector Double
  -> V.Vector MPPCAData
  -> (MPPCAParameter, Double)
updateWSigmaKMPPCA m@(Model (a,PPCA nD nM wMOld _muM sigma')) invM newMu zs xs =
  (newW,trace y2 / P.fromIntegral nD)
  where n = P.fromIntegral $ V.length xs
        diagSigma = diagonal 0 (V.replicate nM sigma')
        ys = assignPointVec m invM zs xs
        s =
          vector2Matrix nD nD .
          VU.map (/ (n * a)) .
          V.foldl1' (VU.zipWith (+)) .
          V.zipWith (\y x ->
                       let xmu = VU.zipWith (-) x newMu
                       in VU.map (* y) (crossProduct xmu))
                    ys $
          xs
        sw = s * wMOld
        x1 = invM * transpose wMOld * sw
        x2 = elementwiseUnsafe (+) x1 diagSigma
        x3 =
          case inverse x2 of
            Left msg -> error msg
            Right x' -> x'
        newW = sw * x3
        y1 = sw * invM * transpose newW
        y2 = elementwiseUnsafe (-) s y1

updateWSigmaMPPCA
  :: ParallelParams
  -> MPPCA
  -> V.Vector (Matrix Double)
  -> V.Vector (VU.Vector Double)
  -> V.Vector Double
  -> V.Vector MPPCAData
  -> (V.Vector MPPCAParameter, V.Vector Double)
updateWSigmaMPPCA parallelParams _model@(MixtureModel _n modelVec) invM newMu zs xs =
  V.unzip $
  parZipWith3ChunkVector
    parallelParams
    rdeepseq
    (\m im mu -> updateWSigmaKMPPCA m im mu zs xs)
    modelVec
    invM
    newMu

em
  :: ParallelParams
  -> FilePath
  -> PPCAInitParams
  -> V.Vector MPPCAData
  -> Double
  -> Double
  -> MPPCA
  -> IO ()
em parallelParams filePath initParams xs threshold oldLikelihood oldModel =
  do (zs,nks,intermediateModel,invM) <-
       computeNKS parallelParams initParams oldModel xs
     let newLikelihood = getLikelihood zs
         (PPCA nD nM _ _ _) =
           snd . (\(Model x) -> x) . V.head . model $ intermediateModel
         avgLikelihood =
           (newLikelihood -
            (P.fromIntegral (V.length xs * nD) / 2) * log (2 * pi)) /
           P.fromIntegral (V.length xs)
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
     time <- getZonedTime
     let timeStr =
           (show . localTimeOfDay . zonedTimeToLocalTime $ time) P.++ ": "
     printf (timeStr P.++ "%0.2f (%0.3f%%)\n")
            avgLikelihood
            ((avgLikelihood - oldLikelihood) / abs oldLikelihood * 100)
     if avgLikelihood > threshold
        then encodeFile filePath intermediateModel
        else do encodeFile filePath intermediateModel
                em parallelParams filePath initParams xs threshold avgLikelihood newModel

mppcaSink
  :: ParallelParams
  -> PPCAInitParams
  -> Int
  -> Double
  -> FilePath
  -> Sink (V.Vector MPPCAData) IO ()
mppcaSink parallelParams initParams numModel' threshold filePath =
  do xs <- CL.take 10
     fileFlag <- liftIO $ doesFileExist filePath
     models <-
       liftIO $
       if fileFlag
          then do fileSize <- liftIO $ getFileSize filePath
                  if fileSize > 0
                     then do putStrLn $ "Read MPPCA data file: " P.++ filePath
                             decodeFile filePath
                     else initializeMPPCA initParams
                                          numModel'
                                          (VU.length . V.head . P.head $ xs)
          else initializeMPPCA initParams
                               numModel'
                               (VU.length . V.head . P.head $ xs)
     let ys = V.concat xs
     liftIO $ em parallelParams filePath initParams ys threshold 0 models
