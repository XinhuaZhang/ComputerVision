import           Application.RotatedMNIST.ArgsParser as AP
import           Application.RotatedMNIST.Conduit
import           Classifier.LibLinear
import           Control.Monad                       as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.FourierMellinTransform
import           CV.Statistics.KMeans
import           CV.Utility.Parallel                 as Par
import           Data.Array.Repa
import           Data.Binary
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.Binary                 as CB
import           Data.Conduit.List                   as CL
import           Data.List                           as L
import           Data.Vector.Unboxed                 as VU
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
      numFreq = 15
      filterParams =
        FourierMellinTransformParamsGrid
        { getFourierMellinTransformGridRows = imageSize params
        , getFourierMellinTransformGridCols = imageSize params
        , getFourierMellinTransformGridRadialFreq = [0 .. fromIntegral numFreq]
        , getFourierMellinTransformGridAngularFreq = [0 .. numFreq]
        }
  writeFile (paramsFileName params) . show $ filterParams
  (plan, filters) <-
    makeFilterConvolution getEmptyPlan filterParams Normal :: IO (DFTPlan, FourierMellinTransformConvolution)
  exportFFTWWisdom (fftwWisdomPath params)
  xs <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    filterConduit parallelParams plan [filters] =$=
    CL.take (numGMMExample params)
  let (ls, ys) = L.unzip xs
  kmeansModel <-
    M.mapM
      (kmeans parallelParams (numGaussian params) (kmeansFile params) 0.005)
      (L.map L.concat . L.transpose $ ys)
  encodeFile (kmeansFile params) kmeansModel
