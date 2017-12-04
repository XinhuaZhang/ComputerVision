import           Application.RotatedMNIST.ArgsParser as AP
import           Application.RotatedMNIST.Conduit
import           Control.Arrow
import           Control.Monad                       as M
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.FourierMellinTransform
import           CV.Filter.ShiftablePinwheelPyramid
import           CV.IO.ImageIO
import           CV.Utility.Draw
import           CV.Utility.Parallel                 as Par
import           Data.Array.Repa                     as R
import           Data.Binary
import           Data.Conduit
import           Data.Conduit.Binary                 as CB
import           Data.Conduit.List                   as CL
import           Data.List                           as L
import           Data.Vector                         as V
import           Data.Vector.Unboxed                 as VU
import           System.Environment


plotImageSink :: Int -> Sink (Double,ShiftablePinwheelPyramidInputArray) (ResourceT IO) ()
plotImageSink n
  | n == 0 = return ()
  | otherwise = do
    x <- await
    case x of
      Nothing -> return ()
      Just (_, y) -> do
        let (Z :. numCenter :. _ :. _ :. _) = extent y
            arrs =
              L.map
                (\i ->
                   Image 8 . computeS . R.slice y $
                   (Z :. i :. All :. All :. All))
                [0 .. numCenter - 1]
        liftIO $
          M.zipWithM_
            (\j arr ->
               plotImageRepa (show n L.++ "_" L.++ show j L.++ ".png") arr)
            [1 ..]
            arrs
        plotImageSink (n - 1)

plotCenterSink :: Int -> Sink ((Double, Double), LabeledArray DIM3 Double) (ResourceT IO) ()
plotCenterSink n
  | n == 0 = return ()
  | otherwise = do
    x <- await
    case x of
      Nothing -> return ()
      Just ((i, j), LabeledArray l arr) -> do
        liftIO . plotImageRepa (show n L.++ ".png") . draw (Image 8 arr) Red $
          [Circle 1 1 (i, j)]
        liftIO . print $ l
        plotCenterSink (n - 1)

main = do
  args <- getArgs
  params <- parseArgs args
  let parallelParams =
        ParallelParams
        { Par.numThread = AP.numThread params
        , Par.batchSize = AP.batchSize params
        }
      filterParams =
        ShiftablePinwheelBlobPyramidParams
        { shiftablePinwheelBlobPyramidNumLayers = 1
        , shiftablePinwheelBlobPyramidNumCenters = (numGrid params) ^ (2 :: Int)
        , shiftablePinwheelBlobPyramidNumChannels = 1
        , shiftablePinwheelBlobPyramidNumTheta = 64
        , shiftablePinwheelBlobPyramidNumLogR = 64
        , shiftablePinwheelBlobPyramidK = 24
        }
      n = 10
  originPredictorParams <-
    fmap (\x -> read x :: FourierMellinTransformParamsGrid) . readFile $
    (originPredictorParamsFileName params)
  (plan, originPredictorFilters) <-
    makeFilterConvolution getEmptyPlan originPredictorParams Normal :: IO (DFTPlan, FourierMellinTransformConvolution)
  runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    getOriginsConduit
      parallelParams
      plan
      [originPredictorFilters]
      (originModelName params) =$=
    plotCenterSink n
  -- runResourceT $
  --   CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
  --   getOriginsConduit
  --     parallelParams
  --     plan
  --     [originPredictorFilters]
  --     (originModelName params) =$=
  --   logpolarImageConduit
  --     parallelParams
  --     (shiftablePinwheelBlobPyramidNumTheta filterParams)
  --     (shiftablePinwheelBlobPyramidNumLogR filterParams)
  --     (radius params)
  --     (logpolarFlag params)
  --     (numGrid params) =$=
  --   plotImageSink n
