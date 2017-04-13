import           Application.FacialExpression.Conduit
import           Classifier.LibLinear
import           Control.Monad                        as M
import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           CV.IO.ImageIO
import           CV.Utility.Parallel
import           CV.V4Filter
import           Data.Array.Repa                      as R
import           Data.Conduit
import           Data.Conduit.List                    as CL
import           Data.List                            as L
import           Data.Set                             as S
import           Data.Vector.Unboxed                  as VU
import           System.Environment

main = do
  (imageListPath:labelListPath:isColorStr:paramsFilePath:_) <- getArgs
  let parallelParams = ParallelParams {numThread = 12, batchSize = 120}
      v4QuardTreeFilterParams =
        V4QuadTreeSeparableFilterParams
        { separableFilterQuadTreeLayer = 1
        , separableFilterRows = n
        , separableFilterCols = n
        , polarSeparableScale = L.map (\i -> 8 * (2 ** (i/2))) [0..5-1]
        , polarSeparableRadialFreq = [8]
        , polarSeparableAngularFreq = [8]
        , polarSeparableName = Pinwheels
        , cartesianSeparableScale = L.map (\i -> 8 * (2 ** (i/2))) [0..5-1]
        , cartesianSeparableXFreq = [0 .. 7]
        , cartesianSeparableYFreq = [0 .. 7]
        , hyperbolicSeparableScale = [32]
        , hyperbolicSeparableUFreq = [0 .. 7]
        , hyperbolicSeparableVFreq = [0 .. 7]
        , hyperbolicSeparableAngle = 15
        , separableFilterParams = PC
        }
      filterVecsList =
        generateV4SeparableFilterQuadTreeFilter v4QuardTreeFilterParams
      n = 128
      downsampleFactor = 1
      isColor = read isColorStr :: Bool
  writeFile paramsFilePath . show $ v4QuardTreeFilterParams
  labels <- readLabelFile labelListPath
  featurePtr <-
    runResourceT $
    imagePathSource imageListPath $$ readImageConduit isColor =$=
    -- CL.map (R.map (\x -> let y = 255 - x
    --                      in if y > 50
    --                            then 1
    --                            else 0)) =$=
    padResizeImageConduit parallelParams n 255 =$=
    applyV4QuadTreeFilterConduit parallelParams filterVecsList =$=
    CL.map (normalizeVec . VU.concat . L.map VU.concat) =$=
    featurePtrConduitP parallelParams =$=
    CL.consume
  let trainParams =
        TrainParams
        { trainSolver = L2R_L2LOSS_SVC_DUAL
        , trainC = 128
        , trainNumExamples = L.length featurePtr
        , trainFeatureIndexMax =
            (L.sum . L.map (\ys -> (L.length . L.head $ ys) * L.length ys) $
             filterVecsList) *
            if isColor
              then 6
              else 2
        , trainModel = "SVM_model"
        }
  print trainParams
  train trainParams labels featurePtr
