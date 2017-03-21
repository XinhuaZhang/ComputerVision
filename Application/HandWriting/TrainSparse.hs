import           Application.HandWriting.Conduit
import           Application.HandWriting.IO
import           Classifier.LibLinear
import           Control.Monad.Trans.Resource
import           CV.Utility.Parallel
import           CV.V4Filter                     hiding
                                                  (applyFilterVariedSizeConduit)
import           Data.Conduit
import           Data.Conduit.List               as CL
import           Data.Conduit.Binary               as CB
import           Data.IntMap.Strict              as IM
import           Data.List                       as L
import           Data.Set                        as S
import           Data.Vector.Unboxed             as VU
import           Data.Word
import           System.Environment

main = do
  (path:_) <- getArgs
  let parallelParams =
        ParallelParams
        { numThread = 32
        , batchSize = 6400
        }
      polarSeparableFilterParamsSet =
        PolarSeparableFilterParamsSet
        { getSizeSet = (n, n)
        , getDownsampleFactorSet = downsampleFactor
        , getScaleSet = S.fromDistinctAscList [16]
        , getRadialFreqSet = S.fromDistinctAscList [0 .. (16 - 1)]
        , getAngularFreqSet = S.fromDistinctAscList [0 .. (8 - 1)]
        , getNameSet = Pinwheels
        }
      cartesianGratingFilterParams =
        CartesianGratingFilterParams
        { getCartesianGratingFilterRows = n
        , getCartesianGratingFilterCols = n
        , getCartesianGratingFilterDownsampleFactor = downsampleFactor
        , getCartesianGratingFilterScale = [24]
        , getCartesianGratingFilterFreq = [0.125, 0.25, 0.5, 1]
        , getCartesianGratingFilterAngle = [0,10 .. 180 - 10]
        }
      hyperbolicFilterParams =
        HyperbolicFilterParams
        { getHyperbolicFilterRows = n
        , getHyperbolicFilterCols = n
        , getHyperbolicFilterDownsampleFactor = downsampleFactor
        , getHyperbolicFilterScale = [24]
        , getHyperbolicFilterFreq = [0.125, 0.25, 0.5, 1]
        , getHyperbolicFilterAngle = [0,10 .. 90 - 10]
        }
      psf =
        makeFilter . changeSizeParameter n n $
        PolarSeparableFilter polarSeparableFilterParamsSet [] :: PolarSeparableFilterExpansion
      cgf =
        makeFilter . changeSizeParameter n n $
        CartesianGratingFilter cartesianGratingFilterParams [] :: CartesianGratingFilter
      hf =
        makeFilter . changeSizeParameter n n $
        HyperbolicFilter hyperbolicFilterParams [] :: HyperbolicFilter
      psfVecs = getFilterVectors psf
      cgfVecs = getFilterVectors cgf
      hfVecs = getFilterVectors hf
      filterVecsList = [psfVecs, cgfVecs, hfVecs]
      n = 128
      downsampleFactor = 1
  labelFeaturePtr <-
    runResourceT $
    CB.sourceFile path $$ sparseOfflineCharacterConduit =$=
    applyFilterfixedSizeSparseConduit parallelParams filterVecsList =$=
    featurePtrConduitP parallelParams =$=
    CL.consume
  let (labels, xs) = L.unzip labelFeaturePtr
      trainParams =
        TrainParams
        { trainSolver = L2R_L2LOSS_SVC_DUAL
        , trainC = 128
        , trainNumExamples = L.length xs
        , trainFeatureIndexMax =
          (getFilterSize
             (PolarSeparableFilter polarSeparableFilterParamsSet [] :: PolarSeparableFilterExpansion) +
           getFilterSize
             (CartesianGratingFilter cartesianGratingFilterParams [] :: CartesianGratingFilter) +
           getFilterSize
             (HyperbolicFilter hyperbolicFilterParams [] :: HyperbolicFilter)) *
          2
        , trainModel = "SVM_model"
        }
  print trainParams
  train trainParams labels xs
