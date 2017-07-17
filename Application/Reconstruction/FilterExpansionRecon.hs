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
      m = 90
      n = 32
      filterParams =
        FourierMellinTransformParamsGrid
        { getFourierMellinTransformGridRows = imageSize
        , getFourierMellinTransformGridCols = imageSize
        , getFourierMellinTransformGridScale = [0]
        , getFourierMellinTransformGridRadialFreq = [fromIntegral (-n), fromIntegral (-n) + 1.. fromIntegral n]
        , getFourierMellinTransformGridAngularFreq = [-n,-n + 1 .. n]
        }
      nullFilter = PolarSeparableFilter filterParams Null
      filters =
        (\(FourierMellinTransform _ vecs) -> L.concatMap L.concat vecs) . getFilterVectors $
        makeFilter nullFilter (div imageSize 2, div imageSize 2)
      cFilterParams =
        FourierMellinTransformParamsGridC
        { getFourierMellinTransformGridCRows = imageSize
        , getFourierMellinTransformGridCCols = imageSize
        , getFourierMellinTransformGridCScale = [0]
        , getFourierMellinTransformGridCRadialFreq = [fromIntegral (-n) .. fromIntegral n]
        , getFourierMellinTransformGridCAngularFreq = [-n .. n]
        }
      cNullFilter = PolarSeparableFilter cFilterParams Null
      cFilters =
        (\(FourierMellinTransform _ vecs) -> L.concatMap L.concat vecs) . getFilterVectors $
        makeFilter cNullFilter (div imageSize 2, div imageSize 2)
  (img:_) <-
    runResourceT $
    sourceList [imagePath] $$ readImageConduit False =$= CL.take 1
  let recon = computeRecon (R.map (\x  -> (x) :+ 0) img) filters -- cFilters
      arr =
        IM.arrayToImage .
        listArray ((0, 0), (imageSize - 1, imageSize - 1)) . R.toList . R.map magnitude $
        recon :: IM.GrayImage
  IM.writeImage "FilterExpansionRecon.pgm" arr
