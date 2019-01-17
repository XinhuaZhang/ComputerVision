import           Application.FacialExpression.Conduit
import           CV.Statistics.PCA
import           Classifier.LibLinear
import           Control.Monad                        as M
import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           CV.IO.ImageIO
import           CV.Utility.Parallel
import           CV.V4Filter
import           Data.Conduit
import           Data.Conduit.List                    as CL
import           Data.List                            as L
import           Data.Set                             as S
import           Data.Vector.Unboxed                  as VU
import           System.Environment
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy

main = do
  (path:_) <- getArgs
  let parallelParams = ParallelParams {numThread = 4, batchSize = 100}
      v4QuardTreeFilterParams =
        V4FilterQuardTreeFilterParams
        { quardTreeLayer = 1
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
      filterVecsList = generateV4FilterQuardTreeFilter v4QuardTreeFilterParams
      filters = L.head filterVecsList
      n = 128
      downsampleFactor = 1
      gridSize = (16, 16)
  labels <- runResourceT $ labelSource' path $$ CL.consume
  landmarks <- runResourceT $ landmarksSource path $$ CL.consume
  features <-
    runResourceT $
    filePathSource path $$ readImageConduit False =$=
    mergeSource (CL.sourceList landmarks) =$=
    cropSquareConduit parallelParams n =$=
    applyFilterFixedSizeComplexConduit parallelParams  filters =$=
    CL.consume
  let (pcaMat, eigenVal, xs) =
        pcaSVD parallelParams (100000) . L.concat $ features
      eigenPairs = VU.toList . VU.imap (\i v -> (i, v)) $ eigenVal
  toFile def "EigenValue.png" $ do
    layout_title .= "Eigenvalue"
    plot (line "" [eigenPairs]) 
