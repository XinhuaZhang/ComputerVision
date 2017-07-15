import           Application.Leaf.ArgsParser      as AP
import           Application.Leaf.Conduit
import           Classifier.LibLinear
import           Control.Monad                    as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
--import           CV.Filter.FourierMellinTransform
--import           CV.Filter.PolarSeparableFilter
import           CV.Filter.PinwheelRing
import           CV.Filter.GaussianFilter
import           CV.Statistics.KMeans
import           CV.Utility.FFT
import           CV.Utility.Parallel              as Par
import           Data.Array.Repa
import           Data.Binary
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.Binary              as CB
import           Data.Conduit.List                as CL
import           Data.List                        as L
import           Data.Vector.Unboxed              as VU
import           System.Environment



main = do
  args <- getArgs
  params <- parseArgs args
  print params
  let parallelParams =
        ParallelParams
        { Par.numThread = AP.numThread params
        , Par.batchSize = AP.batchSize params
        }
      n = 15
      filterParams =
        PinwheelRingParams
                     { pinwheelRingRows         = imageSize params
                     , pinwheelRingCols         = imageSize params
                     , pinwheelGaussianScale    = 1
                     , pinwheelRingScale        = L.map (\x -> sqrt 2 ** x) [0..3]
                     , pinwheelRingRadialFreqs  = 3/4*pi
                     , pinwheelRingAngularFreqs = [-15..15]
                     , pinwheelRingRadius       = [1..8]
                     } 
        -- PolarSeparableFilterParamsGrid
        -- { getPolarSeparableFilterGridRows = imageSize params
        -- , getPolarSeparableFilterGridCols = imageSize params
        -- , getPolarSeparableFilterGridScale = [1]
        -- , getPolarSeparableFilterGridRadialFreq = [0 .. n]
        -- , getPolarSeparableFilterGridAngularFreq = [0 .. n]
        -- }
      --   FourierMellinTransformParamsGrid
      --   { getFourierMellinTransformGridRows = imageSize params
      --   , getFourierMellinTransformGridCols = imageSize params
      --   , getFourierMellinTransformGridRadialFreq =
      --     [0 .. fromIntegral n]
      --   , getFourierMellinTransformGridAngularFreq = [0 .. n]
      --   }
      gFilterParams =
        GaussianFilter1DParams
          (gaussianScale params) 8
      fftwWisdom = FFTWWisdomPath (fftwWisdomPath params)
  writeFile (paramsFileName params) . show $ filterParams
  fftwInit <- initializefftw FFTWWisdomNull
  imgs <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    CL.map (\(LabeledArray _ arr) -> toUnboxed arr) =$=
    CL.take 1
  generateWisdom
    fftwInit
    (fftwWisdomPath params)
    (imageSize params)
    (imageSize params) .
    VU.convert . VU.map (:+ 0) . L.head $
    imgs
  fftw <- initializefftw fftwWisdom
  filters <- makeFilterConvolution fftw filterParams Normal :: IO PinwheelRingConvolution   -- PolarSeparableFilterGridConvolution -- FourierMellinTransformConvolution
  gFilters <- makeFilterConvolution fftw gFilterParams Normal :: IO GaussianFilterConvolution1D
  xs <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    filterConduit parallelParams fftw [filters] gFilters False (stride params) =$=
    -- pinwheelRingGaussianConvolutionConduit parallelParams fftw filters gFilters (stride params) =$=
    CL.take (numGMMExample params)
  let (ls, ys) = L.unzip xs
  kmeansModel <- kmeans parallelParams (numGaussian params) (L.concat ys)
  encodeFile (kmeansFile params) kmeansModel
