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
  (path:modelName:gridSizeStr:_) <- getArgs
  let parallelParams =
        ParallelParams
        { numThread = 12
        , batchSize = 4800
        }
      polarSeparableFilterParamsSet =
        PolarSeparableFilterParamsSet
        { getSizeSet = (n, n)
        , getDownsampleFactorSet = downsampleFactor
        , getScaleSet = S.fromDistinctAscList [4]
        , getRadialFreqSet = S.fromDistinctAscList [0 .. (4 - 1)]
        , getAngularFreqSet = S.fromDistinctAscList [0 .. (4 - 1)]
        , getNameSet = Pinwheels
        }
      polarSeparableFilterParamsSet1 =
        PolarSeparableFilterParamsSet {getSizeSet = (n,n)
                                      ,getDownsampleFactorSet =
                                         downsampleFactor
                                      ,getScaleSet =
                                         S.fromDistinctAscList [8]
                                      ,getRadialFreqSet =
                                         S.fromDistinctAscList [0 .. (8 - 1)]
                                      ,getAngularFreqSet =
                                         S.fromDistinctAscList [0 .. (8 - 1)]
                                      ,getNameSet = Pinwheels}
      cartesianGratingFilterParams =
        CartesianGratingFilterParams
        { getCartesianGratingFilterRows = n
        , getCartesianGratingFilterCols = n
        , getCartesianGratingFilterDownsampleFactor = downsampleFactor
        , getCartesianGratingFilterScale = [12]
        , getCartesianGratingFilterFreq = [0.125,0.25, 0.5, 1]
        , getCartesianGratingFilterAngle = [0,10 .. 180 - 10]
        }
      hyperbolicFilterParams =
        HyperbolicFilterParams
        { getHyperbolicFilterRows = n
        , getHyperbolicFilterCols = n
        , getHyperbolicFilterDownsampleFactor = downsampleFactor
        , getHyperbolicFilterScale = [12]
        , getHyperbolicFilterFreq = [0.125,0.25, 0.5, 1]
        , getHyperbolicFilterAngle = [0,10 .. 90 - 10]
        }
      psf =
        makeFilterGrid gridSize . changeSizeParameter n n $
        PolarSeparableFilter polarSeparableFilterParamsSet [] :: PolarSeparableFilterExpansion
      psf1 =
        makeFilterGrid gridSize . changeSizeParameter n n $
        PolarSeparableFilter polarSeparableFilterParamsSet1 [] :: PolarSeparableFilterExpansion
      cgf =
        makeFilterGrid gridSize . changeSizeParameter n n $
        CartesianGratingFilter cartesianGratingFilterParams [] :: CartesianGratingFilter
      hf =
        makeFilterGrid gridSize . changeSizeParameter n n $
        HyperbolicFilter hyperbolicFilterParams [] :: HyperbolicFilter
      psfVecs = getFilterVectors psf
      psfVecs1 = getFilterVectors psf1
      cgfVecs = getFilterVectors cgf
      hfVecs = getFilterVectors hf
      filterVecsList = [psfVecs1,cgfVecs,hfVecs]
      n = 128
      downsampleFactor = 1
      gridSize = read gridSizeStr :: (Int,Int)
  runResourceT $
    CB.sourceFile path $$ sparseOfflineCharacterConduit =$=
    applyFilterfixedSizeSparseConduit parallelParams filterVecsList =$=
    featureConduitP parallelParams =$=
    predict modelName (modelName L.++ ".out")
