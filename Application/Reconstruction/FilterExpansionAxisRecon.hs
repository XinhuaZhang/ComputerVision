import           Application.Reconstruction.Recon
import           Control.Monad.Trans.Resource
import           CV.IO.ImageIO
import           CV.V4Filter
import           Data.Array                       as Arr
import           Data.Array.Repa                  as R
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.List                as CL
import qualified Data.Image                       as IM
import           Data.List                        as L
import           Data.Vector.Unboxed              as VU
import           System.Environment

main = do
  (imagePath:imageSizeStr:_) <- getArgs
  let imageSize = read imageSizeStr :: Int
      m = 10
      n = 48
      filterParams =
        V4SeparableFilterParamsAxis
        { v4SeparableFilterParamsAxisSeparableFilterRows = imageSize
        , v4SeparableFilterParamsAxisSeparableFilterCols = imageSize
        , v4SeparableFilterParamsAxisPolarSeparablePolarFactor = 2
        , v4SeparableFilterParamsAxisPolarSeparableScale = [56]
        , v4SeparableFilterParamsAxisPolarSeparableFreq = [-n .. n]
        , v4SeparableFilterParamsAxisPolarSeparableAngle = [0, m .. 90 - m]
        , v4SeparableFilterParamsAxisCartesianGratingScale =
          [ 2 ** (i / 2)
          | i <- [7 .. 10] ]
        , v4SeparableFilterParamsAxisCartesianGratingFreq = L.take 8 [1 .. 8]
        , v4SeparableFilterParamsAxisCartesianGratingAngle = [0,15 .. 360 - 15]
        , v4SeparableFilterParamsAxisHyperbolicSeparableScale =
          [ 2 ** (i / 2)
          | i <- [7 .. 10] ]
        , v4SeparableFilterParamsAxisHyperbolicSeparableUFreq = [0 .. 3]
        , v4SeparableFilterParamsAxisHyperbolicSeparableVFreq = [0 .. 3]
        , v4SeparableFilterParamsAxisHyperbolicSeparableAngle = 15
        , v4SeparableFilterParamsAxisSeparableFilterParams = P
        }
      filters =
        (\(V4PolarSeparableFilterAxis _ vecs) -> L.concat vecs) . L.head . generateV4SeparableFilterAxis $
        filterParams
  print . L.length $ filters
  (img:_) <-
    runResourceT $
    sourceList [imagePath] $$ readImageConduit False =$= CL.take 1
  let recon = computeRecon (R.map (\x -> x :+ 0) img) filters
      arr =
        IM.arrayToImage .
        listArray ((0, 0), (imageSize - 1, imageSize - 1)) . R.toList $
        recon :: IM.ComplexImage
  IM.writeImage "FilterExpansionRecon.pgm" arr
