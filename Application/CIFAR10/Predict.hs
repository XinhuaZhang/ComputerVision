import           Application.FacialExpression.Conduit
import           Application.RotateDataset.RotationRepa
import           Classifier.LibLinear
import           Control.Monad                        as M
import           Control.Monad.Trans.Resource
import           CV.IO.ImageIO
import           CV.Utility.Parallel
import           CV.V4Filter
import           Data.Conduit
import           Data.Conduit.List                    as CL
import           Data.List                            as L
import           Data.Set                             as S
import           Data.Vector.Unboxed                  as VU
import           System.Environment

main = do
  (imageListPath:labelListPath:isColorStr:gridSizeStr:_) <- getArgs
  let parallelParams = ParallelParams {numThread = 12, batchSize = 1200}
      v4QuardTreeFilterParams =
        V4QuadTreeSeparableFilterParams
        { separableFilterQuadTreeLayer = 2
        , separableFilterRows = n
        , separableFilterCols = n
        , polarSeparableScale = [16]
        , polarSeparableRadialFreq = [16, 8]
        , polarSeparableAngularFreq = [16, 8]
        , polarSeparableName = Pinwheels
        , cartesianSeparableScale = [28]
        , cartesianSeparableXFreq = [0,0.05 .. 1]
        , cartesianSeparableYFreq = [0,0.05 .. 1]
        , hyperbolicSeparableScale = [28]
        , hyperbolicSeparableUFreq = [0,0.5 .. 4]
        , hyperbolicSeparableVFreq = [0,0.1 .. 1]
        , hyperbolicSeparableAngle = 15
        }
      filterVecsList =
        generateV4SeparableFilterQuadTreeFilter v4QuardTreeFilterParams
      n = 128
      downsampleFactor = 1
      isColor = read isColorStr :: Bool
      gridSize = read gridSizeStr :: (Int, Int)
  runResourceT $
    imagePathSource imageListPath $$ readImageConduit isColor =$=
    resizeImageConduit parallelParams n =$=
    applyV4QuadTreeFilterConduit parallelParams filterVecsList =$=
    CL.map (normalizeVec . VU.concat . L.map VU.concat) =$=
    featureConduitP parallelParams =$=
    mergeSource (labelSource labelListPath) =$=
    predict "SVM_model" "SVM_model.out"
