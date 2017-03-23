import           Application.HandWriting.Conduit
import           Application.HandWriting.IO
import           Classifier.LibLinear
import           Control.Arrow
import           Control.Monad.Trans.Resource
import           CV.Utility.Parallel
import           CV.V4Filter                     hiding
                                                  (applyFilterVariedSizeConduit)
import           Data.Conduit
import           Data.Conduit.Binary             as CB
import           Data.Conduit.List               as CL
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
      polarSeparableFilterParams =
        PolarSeparableFilterParamsGrid
        { getPolarSeparableFilterGridRows = fst gridSize
        , getPolarSeparableFilterGridCols = snd gridSize
        , getPolarSeparableFilterRows = n
        , getPolarSeparableFilterCols = n
        , getPolarSeparableFilterDownsampleFactor = downsampleFactor
        , getPolarSeparableFilterScale = [4]
        , getPolarSeparableFilterRadialFreq = [0 .. (4 - 1)]
        , getPolarSeparableFilterAngularFreq = [0 .. (4 - 1)]
        , getPolarSeparableFilterName = Pinwheels
        }
      polarSeparableFilterParams1 =
        PolarSeparableFilterParamsGrid
        { getPolarSeparableFilterGridRows = fst gridSize
        , getPolarSeparableFilterGridCols = snd gridSize
        , getPolarSeparableFilterRows = n
        , getPolarSeparableFilterCols = n
        , getPolarSeparableFilterDownsampleFactor = downsampleFactor
        , getPolarSeparableFilterScale = [8]
        , getPolarSeparableFilterRadialFreq = [0 .. (8 - 1)]
        , getPolarSeparableFilterAngularFreq = [0 .. (8 - 1)]
        , getPolarSeparableFilterName = Pinwheels
        }
      cartesianGratingFilterParams =
        CartesianGratingFilterParams
        { getCartesianGratingFilterGridRows = fst gridSize
        , getCartesianGratingFilterGridCols = snd gridSize
        , getCartesianGratingFilterRows = n
        , getCartesianGratingFilterCols = n
        , getCartesianGratingFilterDownsampleFactor = downsampleFactor
        , getCartesianGratingFilterScale = [12]
        , getCartesianGratingFilterFreq = [0.125, 0.25, 0.5, 1]
        , getCartesianGratingFilterAngle = [0,10 .. 180 - 10]
        }
      hyperbolicFilterParams =
        HyperbolicFilterParams
        { getHyperbolicFilterGridRows = fst gridSize
        , getHyperbolicFilterGridCols = snd gridSize
        , getHyperbolicFilterRows = n
        , getHyperbolicFilterCols = n
        , getHyperbolicFilterDownsampleFactor = downsampleFactor
        , getHyperbolicFilterScale = [12]
        , getHyperbolicFilterFreq = [0.125, 0.25, 0.5, 1]
        , getHyperbolicFilterAngle = [0,10 .. 90 - 10] --[0, 45, 90]
        }
      psf =
        makeFilter . changeSizeParameter n n $
        PolarSeparableFilter polarSeparableFilterParams [] :: PolarSeparableFilterExpansion
      psf1 =
        makeFilter . changeSizeParameter n n $
        PolarSeparableFilter polarSeparableFilterParams1 [] :: PolarSeparableFilterExpansion
      cgf =
        makeFilter . changeSizeParameter n n $
        CartesianGratingFilter cartesianGratingFilterParams [] :: CartesianGratingFilter
      hf =
        makeFilter . changeSizeParameter n n $
        HyperbolicFilter hyperbolicFilterParams [] :: HyperbolicFilter
      psfVecs = getFilterVectors psf
      psfVecs1 = getFilterVectors psf1
      cgfVecs = getFilterVectors cgf
      hfVecs = getFilterVectors hf
      filterVecsList = L.zipWith3 (\a b c -> a L.++ b L.++ c) psfVecs1 cgfVecs hfVecs
      n = 128
      downsampleFactor = 1
      gridSize = read gridSizeStr :: (Int, Int)
  runResourceT $
    CB.sourceFile path $$ sparseOfflineCharacterConduit =$=
    applyFilterfixedSizeSparseConduit parallelParams filterVecsList =$=
    CL.map (second VU.concat) =$=
    featureConduitP parallelParams =$=
    predict modelName (modelName L.++ ".out")
