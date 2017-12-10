import           Application.CaffeData.HDF5
import           Application.RotatedMNIST.ArgsParser as AP
import           Application.RotatedMNIST.Conduit
import           Control.Monad                       as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.FourierMellinTransform
import           CV.Filter.ShiftablePinwheelPyramid
import           CV.Utility.Parallel                 as Par
import           Data.Array.Repa                     as R
import           Data.Conduit
import           Data.Conduit.Binary                 as CB
import           Data.Conduit.List
import           Data.List                           as L
import           System.Environment
import           System.FilePath

toLabeledArrayConduit
  :: Conduit (Double, ShiftablePinwheelPyramidInputArray) (ResourceT IO) [LabeledArray DIM3 Double]
toLabeledArrayConduit =
  awaitForever
    (\(label, arr) ->
       let (Z :. _ :. nc :. _ :. _) = extent arr
       in sourceList .
          L.map
            (\i ->
               [ LabeledArray (round label) . computeS . R.slice arr $
                 (Z :. All :. i :. All :. All)
               ]) $
          [0 .. nc - 1])

main = do
  args <- getArgs
  params <- parseArgs args
  print params
  filterParams <-
    fmap (\x -> read x :: ShiftablePinwheelBlobPyramidParams) . readFile $
    (paramsFileName params)
  originPredictorParams <-
    fmap (\x -> read x :: FourierMellinTransformParamsGrid) . readFile $
    (originPredictorParamsFileName params)
  (plan, originPredictorFilters) <-
    makeFilterConvolution getEmptyPlan originPredictorParams Normal :: IO (DFTPlan, FourierMellinTransformConvolution)
  let parallelParams =
        ParallelParams
        { Par.numThread = AP.numThread params
        , Par.batchSize = AP.batchSize params
        }
  runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    -- getOriginsConduit
    --  parallelParams
    --   plan
    --   [originPredictorFilters]
    --   (originModelName params) =$=
    centerConduit =$=
    logpolarImageConduit
      parallelParams
      64 -- (shiftablePinwheelBlobPyramidNumTheta filterParams)
      64 -- (shiftablePinwheelBlobPyramidNumLogR filterParams)
      (radius params)
      (logpolarFlag params)
      (numGrid params) =$=
    toLabeledArrayConduit =$=
    hdf5Sink parallelParams ("Pixel" </> (takeBaseName . inputFile $ params) )

