import           Control.Monad       as M
import           CV.V4Filter
import           Data.Array          as Arr
import qualified Data.Image          as IM
import           Data.List           as L
import           Data.Vector.Unboxed as VU

main = do
  let (nx, ny) = (48, 48)
      hyperbolicSeparableFilterParams =
        HyperbolicSeparableFilterParams
        { getHyperbolicSeparableFilterGridRows = 1
        , getHyperbolicSeparableFilterGridCols = 1
        , getHyperbolicSeparableFilterRows = ny
        , getHyperbolicSeparableFilterCols = nx
        , getHyperbolicSeparableFilterDownsampleFactor = 1
        , getHyperbolicSeparableFilterScale = [10]
        , getHyperbolicSeparableFilterUFreq = [0 .. 8]
        , getHyperbolicSeparableFilterVFreq = [0 .. 6]
        , getHyperbolicSeparableFilterAngle = [0]
        }
      hyperbolicFilters =
        L.head $
        getFilterVectors
          (makeFilter $ HyperbolicSeparableFilter hyperbolicSeparableFilterParams [] :: HyperbolicSeparableFilter)
      cartesianSeparableFilterParams =
        CartesianSeparableFilterParams
        { getCartesianSeparableFilterGridRows = 1
        , getCartesianSeparableFilterGridCols = 1
        , getCartesianSeparableFilterRows = ny
        , getCartesianSeparableFilterCols = nx
        , getCartesianSeparableFilterDownsampleFactor = 1
        , getCartesianSeparableFilterScale = [10]
        , getCartesianSeparableFilterXFreq = [0..4]
        , getCartesianSeparableFilterYFreq = [0..4]
        }
      cartesianSeparableFilters = L.head $
        getFilterVectors
          (makeFilter $ CartesianSeparableFilter cartesianSeparableFilterParams [] :: CartesianSeparableFilter)
      imgList =
        L.map
          (IM.arrayToImage . listArray ((0, 0), (ny - 1, nx - 1)) . VU.toList)
          cartesianSeparableFilters
          -- hyperbolicFilters
        :: [IM.ComplexImage]
  M.zipWithM_ (\i img -> IM.writeImage (show i L.++ ".ppm") img) [1 ..] imgList
