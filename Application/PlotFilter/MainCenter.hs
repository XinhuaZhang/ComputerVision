import           Control.Monad       as M
import           CV.V4Filter
import           Data.Array          as Arr
import qualified Data.Image          as IM
import           Data.List           as L
import           Data.Vector.Unboxed as VU

main = do
  let (nx, ny) = (32, 32)
      hyperbolicFilterParams =
        HyperbolicFilterParams
        { getHyperbolicFilterRows = ny
        , getHyperbolicFilterCols = nx
        , getHyperbolicFilterDownsampleFactor = 1
        , getHyperbolicFilterScale = [10]
        , getHyperbolicFilterFreq = [0.5,1,1.5]
        , getHyperbolicFilterAngle = [0]
        }
      hyperbolicFilters =
        getFilterVectors
          (makeFilter $ HyperbolicFilter hyperbolicFilterParams [] :: HyperbolicFilter)
      cartesianGratingFilterParams =
        CartesianGratingFilterParams
        { getCartesianGratingFilterRows = ny
        , getCartesianGratingFilterCols = nx
        , getCartesianGratingFilterDownsampleFactor = 1
        , getCartesianGratingFilterScale = [10]
        , getCartesianGratingFilterFreq = [0.125,0.25,0.5,1]
        , getCartesianGratingFilterAngle = [0]
        }
      cartesianGratingFilters =
        getFilterVectors
          (makeFilter $ CartesianGratingFilter cartesianGratingFilterParams [] :: CartesianGratingFilter)
      imgList =
        L.map
          (IM.arrayToImage . listArray ((0, 0), (ny - 1, nx - 1)) . VU.toList)
          --cartesianGratingFilters 
          hyperbolicFilters:: [IM.ComplexImage]
  M.zipWithM_ (\i img -> IM.writeImage (show i L.++ ".ppm") img) [1 ..] imgList
