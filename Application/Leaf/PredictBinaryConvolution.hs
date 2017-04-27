{-# LANGUAGE BangPatterns #-}
import           Application.Leaf.Conduit
import           Application.RecenterImage.Conduit
import           Classifier.LibLinear
import           Control.Arrow
import           Control.DeepSeq
import           Control.Monad                     as M
import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           CV.Array.LabeledArray
import           CV.Filter.GaussianFilter          as Gaussian
import           CV.IO.ImageIO
import           CV.Utility.Parallel
import           CV.V4Filter
import           CV.V4FilterConvolution
import           Data.Array.Repa                   as R
import           Data.Conduit
import           Data.Conduit.Binary               as CB
import           Data.Conduit.List                 as CL
import           Data.List                         as L
import           Data.Set                          as S
import           Data.Vector.Unboxed               as VU
import           System.Environment


main = do
  (imageListPath:isColorStr:paramsFilePath:sizeStr:modelName:_) <- getArgs
  filterParams <-
    fmap (\x -> read x :: V4SeparableFilterParamsAxis) . readFile $ paramsFilePath
  let parallelParams =
        ParallelParams
        { numThread = 8
        , batchSize = 160
        }
      (rows, cols) = read sizeStr :: (Int, Int)
      isColor = read isColorStr :: Bool
      filters = generateV4SeparableFilterAxis filterParams
  filtersF <- M.mapM (fourierTransformFilter (rows, cols)) filters
  runResourceT $
    CB.sourceFile imageListPath $$ readLabeledImagebinaryConduit =$=
    applyV4SeparableFilterConvolutionLabeledArrayConduit filtersF =$=
    calculateV4SeparableFilterConvolutionFeatureConduit parallelParams =$=
    featureConduitP parallelParams =$=
    predict modelName (modelName L.++ ".out")
