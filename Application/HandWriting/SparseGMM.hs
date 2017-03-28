import           Application.HandWriting.Conduit
import           Application.HandWriting.IO
import           Application.MultiDimensionalGMM.GMM
import           Classifier.LibLinear
import           Control.Arrow
import           Control.Monad                       as M
import           Control.Monad.Trans.Resource
import           CV.Statistics.PCA
import           CV.Utility.Parallel
import           CV.V4Filter                         hiding (applyFilterVariedSizeConduit,
                                                      applyV4QuadTreeFilterConduit)
import           Data.Binary
import           Data.Conduit
import           Data.Conduit.Binary                 as CB
import           Data.Conduit.List                   as CL
import           Data.IntMap.Strict                  as IM
import           Data.List                           as L
import           Data.Set                            as S
import           Data.Vector.Unboxed                 as VU
import           Data.Word
import           System.Environment
import           System.IO
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy

main = do
  (path:numLayerStr:imageSizeStr:gmmFile:numGaussianStr:thresholdStr:pcaFile:numPrincipalStr:numTrainStr:filterParamsPath:numThreadStr:numBatchSizeStr:_) <-
    getArgs
  let parallelParams =
        ParallelParams
        { numThread = read numThreadStr :: Int
        , batchSize = read numBatchSizeStr :: Int
        }
      v4QuadTreeFilterParams =
        V4QuadTreeFilterParams
        { quadTreeLayer = read numLayerStr :: Int
        , rows = n
        , cols = n
        , polarSeparableFilterScale = [16]
        , polarSeparableFilterRadialFreq = [16, 8, 4]
        , polarSeparableFilterAngularFreq = [8, 8, 4]
        , polarSeparableFilterName = Pinwheels
        , cartesianGratingFilterScale = [24]
        , cartesianGratingFilterFreq = [0.125, 0.25, 0.5, 1]
        , cartesianGratingFilterAngle = 10
        , hyperbolicFilterFilterScale = [24]
        , hyperbolicFilterFilterFreq = [0.125, 0.25, 0.5, 1]
        , hyperbolicFilterFilterAngle = 10
        }
      filterVecsList = generateV4FilterQuadTreeFilter v4QuadTreeFilterParams
      n = read imageSizeStr :: Int
  writeFile filterParamsPath . show $ v4QuadTreeFilterParams
  labelFeature <-
    runResourceT $
    CB.sourceFile path $$ sparseOfflineCharacterConduit =$=
    applyV4QuadTreeFilterComplexConduit parallelParams filterVecsList =$=
    CL.consume
  let features = L.transpose . snd . L.unzip $ labelFeature
      (pcaMat, eigenVal, xs) =
        L.unzip3 .
        L.map
          (pcaSVD parallelParams (read numPrincipalStr :: Int) .
           L.take (read numTrainStr :: Int) . L.concat) $
        features
      eigenPairs = VU.toList . VU.imap (\i v -> (i, v)) . L.head $ eigenVal
  -- (pcaMat, _, xs) <-
  --   fmap L.unzip3 .
  --   M.mapM
  --     (pcaCovariance parallelParams (read numPrincipalStr :: Int) .
  --      L.take (read numTrainStr :: Int) . L.concat) $
  --   features
  toFile def "EigenValue.png" $ do
    layout_title .= "Eigenvalue"
    plot (line "" [eigenPairs])
  encodeFile pcaFile pcaMat
  -- withBinaryFile gmmFile WriteMode $ \h ->
  --   M.mapM_
  --     (\feature ->
  --        runResourceT $
  --        sourceList feature $$ CL.map (normalizeVec . complexVec2RealVec) =$=
  --        hGMMSink2
  --          parallelParams
  --          h
  --          (read numGaussianStr :: Int)
  --          (read thresholdStr :: Double)
  --          (read numTrainStr :: Int))
  --     xs
