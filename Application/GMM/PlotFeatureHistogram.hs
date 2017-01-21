import           Application.FilterStats.FilterStats
import           Application.GMM.ArgsParser     as Parser
import           Application.MultiDimensionalGMM.GMM
import           Application.GMM.PCA
import           Control.Monad                  as M
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Feature.PolarSeparable
import           CV.Filter.PolarSeparableFilter
import           CV.Utility.Parallel            as Parallel
import           Data.Array.Repa                as R
import           Data.Binary
import           Data.ByteString.Lazy           as BL
import           Data.Conduit
import           Data.Conduit.Binary            as CB
import           Data.Conduit.List              as CL
import           Data.List                      as L
import           Data.Set                       as S
import           Numeric.LinearAlgebra.Data     as LA
import           Prelude                        as P
import           System.Directory
import           System.Environment
import           System.IO                      as IO
import           Data.Vector.Unboxed                 as VU

splitList :: [Int] -> [a] -> [[a]]
splitList [] _ = []
splitList (i:is) xs = as : splitList is bs
  where (as,bs) = L.splitAt i xs 

plotHistSink :: String -> Sink [[VU.Vector Double]] (ResourceT IO) ()
plotHistSink str = do
  xs <- CL.consume
  let index = L.map (VU.length . L.head) . L.head $ xs
      ys =
        L.map VU.concat .
        L.transpose .
        L.map (L.concatMap (L.map VU.fromList . L.transpose . L.map VU.toList)) $
        xs
  liftIO $
    M.zipWithM_
      (\j zs ->
         M.zipWithM_
           (\i vec ->
              plotHist
                vec
                (VU.minimum vec, VU.maximum vec)
                1000
                (show i)
                (str L.++ show j L.++ "_" L.++ show i L.++ ".png"))
           [1 ..]
           zs)
      [1 ..]
      (splitList index ys)


main = do
  args <- getArgs
  if P.null args
    then error "run with --help to see options."
    else return ()
  params <- parseArgs args
  print params
  imageSize <-
    if isFixedSize params
      then do
        xs <-
          runResourceT $
          sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
          CL.take 1
        let (LabeledArray _ arr) = L.head xs
            (Z :. _ :. ny :. nx) = extent arr
        return (ny, nx)
      else return (0, 0)
  pcaMatrixes <- readMatrixes (pcaFile params)
  images <- readLabeledImageBinary (inputFile params) (numGMMExample params)
  let parallelParams =
        ParallelParams
        { Parallel.numThread = Parser.numThread params
        , Parallel.batchSize = Parser.batchSize params
        }
      filterParamsSet1 =
        PolarSeparableFilterParamsSet
        { getSizeSet = imageSize
        , getDownsampleFactorSet = 1
        , getScaleSet = S.fromDistinctAscList (scale params)
        , getRadialFreqSet = S.fromDistinctAscList [0 .. (freq params - 1)]
        , getAngularFreqSet = S.fromDistinctAscList [0 .. (freq params - 1)]
        , getNameSet = Pinwheels
        }
      filterParamsSet2 =
        PolarSeparableFilterParamsSet
        { getSizeSet = imageSize
        , getDownsampleFactorSet = 2
        , getScaleSet = S.fromDistinctAscList (scale params)
        , getRadialFreqSet = S.fromDistinctAscList [0 .. (freq params - 1)]
        , getAngularFreqSet = S.fromDistinctAscList [0 .. (freq params - 1)]
        , getNameSet = Pinwheels
        }
      filterParamsSetList =
        [filterParamsSet1, filterParamsSet2]
      magnitudeConduit =
        if isFixedSize params
          then multiLayerMagnitudeFixedSizedConduit
                 parallelParams
                 (L.map makeFilterSet filterParamsSetList)
                 (downsampleFactor params)
          else multiLayerMagnitudeVariedSizedConduit
                 parallelParams
                 filterParamsSetList
                 (downsampleFactor params)
  -- runResourceT $
  --   sourceList images $$ magnitudeConduit =$= CL.map snd =$= plotHistSink ""
  runResourceT $
    sourceList images $$ magnitudeConduit =$=
    pcaLabelMultiLayerConduit parallelParams pcaMatrixes =$=
    CL.map snd =$=
    plotHistSink "pca_"
