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
  (imageListPath:labelListPath:isColorStr:_) <- getArgs
  let parallelParams =
        ParallelParams
        { numThread = 16
        , batchSize = 3200
        }
      polarSeparableFilterParamsSet =
        PolarSeparableFilterParamsSet
        { getSizeSet = (n, n)
        , getDownsampleFactorSet = downsampleFactor
        , getScaleSet = S.fromDistinctAscList [4,6,8]
        , getRadialFreqSet = S.fromDistinctAscList [0 .. (6 - 1)]
        , getAngularFreqSet = S.fromDistinctAscList [0 .. (6 - 1)]
        , getNameSet = Pinwheels
        }
      cartesianGratingFilterParams =
        CartesianGratingFilterParams
        { getCartesianGratingFilterRows = n
        , getCartesianGratingFilterCols = n
        , getCartesianGratingFilterDownsampleFactor = downsampleFactor
        , getCartesianGratingFilterScale = [4, 8,16]
        , getCartesianGratingFilterFreq = [0.125, 0.25, 0.5, 1]
        , getCartesianGratingFilterAngle = [0,10 .. 360 - 10]
        }
      hyperbolicFilterParams =
        HyperbolicFilterParams
        { getHyperbolicFilterRows = n
        , getHyperbolicFilterCols = n
        , getHyperbolicFilterDownsampleFactor = downsampleFactor
        , getHyperbolicFilterScale = [4,  8, 16]
        , getHyperbolicFilterFreq = [0.5, 1, 1.5]
        , getHyperbolicFilterAngle = [0,10 .. 90 - 10]
        }
      polarSeparableFilter =
        getFilterVectors
          (makeFilter $ PolarSeparableFilter polarSeparableFilterParamsSet [] :: PolarSeparableFilterExpansion)
      cartesianGratingFilter =
        getFilterVectors
          (makeFilter $ CartesianGratingFilter cartesianGratingFilterParams [] :: CartesianGratingFilter)
      hyperbolicFilter =
        getFilterVectors
          (makeFilter $ HyperbolicFilter hyperbolicFilterParams [] :: HyperbolicFilter)
      filters = [polarSeparableFilter, cartesianGratingFilter, hyperbolicFilter]
      n = 64
      downsampleFactor = 1
      isColor = read isColorStr :: Bool
  labels <- readLabelFile labelListPath
  runResourceT $
    imagePathSource imageListPath $$ readImageConduit isColor =$=
    resizeImageConduit parallelParams n =$=
    applyFilterFixedSizeConduit parallelParams downsampleFactor filters =$=
    featureConduitP parallelParams =$=
    mergeSource (labelSource labelListPath) =$=
    predict "SVM_model" "SVM_model.out"
