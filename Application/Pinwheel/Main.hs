import           Application.Pinwheel.Reconstruction
import           Control.Monad                       as M
import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           CV.Array.LabeledArray
import           CV.Feature.PolarSeparable
import           CV.Filter.PolarSeparableFilter
import           CV.Utility.Parallel
import           Data.Array                          as Arr
import           Data.Array.Repa                     as R
import           Data.Conduit
import           Data.Conduit.List                   as CL
import           Data.Image
import           Data.List                           as L
import           Data.Set                            as S
import           System.Environment


main = do
  (inputPath:learningRate:count:_) <- getArgs
  let parallelParams =
        ParallelParams
        { numThread = 2
        , batchSize = 4
        }
      filterParamsSet =
        PolarSeparableFilterParamsSet
        { getSizeSet = (0, 0)
        , getDownsampleFactorSet = 1
        , getScaleSet = S.fromDistinctAscList [2]
        , getRadialFreqSet = S.fromDistinctAscList [0 .. (4 - 1)]
        , getAngularFreqSet = S.fromDistinctAscList [0 .. (4 - 1)]
        , getNameSet = Pinwheels
        }
      filters = (generateComplexFilters filterParamsSet)
  images <- readLabeledImageBinary inputPath 1
  let image = (\(LabeledArray _ arr) -> arr) . L.head $ images
      imgExtent = extent image
  act <-
    computeActivityComplex
      (read learningRate :: Double)
      (read count :: Int)
      filters
      image
  plotImage "image.png" image
  plotComplexImage "." . computeReconComplex filters $ act