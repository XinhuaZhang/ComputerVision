import           Control.Monad                    as M
import           CV.Filter.CartesianGratingFilter as CF
import           CV.Filter.HyperbolicFilter       as HF
import           Data.Array                       as Arr
import           Data.Image
import           Data.List                        as L
import           Data.Vector.Unboxed              as VU

main = do
  let (nx, ny) = (128, 128)
      -- filterParams =
      --   HyperbolicFilterParams
      --   { getHyperbolicFilterSize = (nx, ny)
      --   , getHyperbolicFilterDownsampleFactor = 1
      --   , getHyperbolicFilterScale = [24,48]
      --   , getHyperbolicFilterFreq = [0.25, 0.5, 1]
      --   , getHyperbolicFilterAngle = [0, 45, 90]
      --   }
      -- (HyperbolicFilter _ filters) = HF.makeFilter filterParams
      filterParams =
        CartesianGratingFilterParams
        { getCartesianGratingFilterSize = (nx, ny)
        , getCartesianGratingFilterDownsampleFactor = 1
        , getCartesianGratingFilterScale = [12,24]
        , getCartesianGratingFilterFreq = [0.125,0.25, 0.5, 1]
        , getCartesianGratingFilterAngle = [0, 45, 90,135]
        }
      (CartesianGratingFilter _ filters) = CF.makeFilter filterParams
      imgList =
        L.map
          (arrayToImage . listArray ((0, 0), (ny - 1, nx - 1)) . VU.toList)
          filters :: [ComplexImage]
  M.zipWithM_ (\i img -> writeImage (show i L.++ ".ppm") img) [1 ..] imgList
