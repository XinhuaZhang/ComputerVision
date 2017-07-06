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
  (imagePath:imageSizeStr:lrStr:writeStepStr:_) <- getArgs
  let imageSize = read imageSizeStr :: Int
      m = 90
      n = 8
      filterParams =
        FourierMellinTransformParamsGrid
        { getFourierMellinTransformGridRows = imageSize
        , getFourierMellinTransformGridCols = imageSize
        , getFourierMellinTransformGridScale = [0]
        , getFourierMellinTransformGridRadialFreq =
          [fromIntegral (-n) .. fromIntegral n]
        , getFourierMellinTransformGridAngularFreq = [ (-n) .. n]
        }
      nullFilter = PolarSeparableFilter filterParams Null
      filters =
        (\(FourierMellinTransform _ vecs) -> L.concatMap L.concat vecs) . getFilterVectors $
        makeFilter nullFilter (div imageSize 2, div imageSize 2)
  (img:_) <-
    runResourceT $
    sourceList [imagePath] $$ readImageConduit False =$= CL.take 1
  recon <-
    magnitudeRecon
      imageSize
      imageSize
      (1 * (0.1 ** (read lrStr :: Double)))
      0.001
      (normalizeImageU (-1,1) . VU.map (:+ 0) . toUnboxed . computeUnboxedS $ img)
      filters
      (read writeStepStr :: Int)
  let arr =
        IM.arrayToImage .
        listArray ((0, 0), (imageSize - 1, imageSize - 1)) . VU.toList $
        recon :: IM.ComplexImage
  IM.writeImage "MagnitudeReconComplex.pgm" arr
  IM.writeImage "MagnitudeRecon.pgm" . IM.magnitude $ arr
