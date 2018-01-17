import           Control.Monad                  as M
import           CV.Filter.PolarSeparableFilter
import           CV.Utility.Coordinates
import           Data.Array                     as Arr
import           Data.Complex
import qualified Data.Image                     as IM
import           Data.List                      as L
import           Data.Vector.Unboxed            as VU
import           System.Directory
import           System.Environment
import           System.FilePath
import           Text.Printf

{-# INLINE filterParamsFunc #-}

filterParamsFunc :: Int
                 -> Int
                 -> String
                 -> PolarSeparableFilterParams
filterParamsFunc rows cols "FourierMellin" =
  FourierMellinTransformParams
  { getFourierMellinTransformRows = rows
  , getFourierMellinTransformCols = cols
  , getFourierMellinTransformRadialFreq = [0 .. 7]
  , getFourierMellinTransformAngularFreq = [0 .. 7]
  }
filterParamsFunc rows cols "GaussianPinwheel" =
  GaussianPinwheelParams
  { getGaussianPinwheelRows = rows
  , getGaussianPinwheelCols = cols
  , getGaussianPinwheelScale = L.map (* pi) [0.25]
  , getGaussianPinwheelRadialFreq = [0 .. 2]
  , getGaussianPinwheelAngularFreq = [-2 .. 2]
  }
filterParamsFunc rows cols "PinwheelFan" =
  PinwheelFanParams
  { pinwheelFanRows = rows
  , pinwheelFanCols = cols
  , pinwheelFanGaussianScale = 0.1 * pi
  , pinwheelFanScale = L.map (\x -> 2 ** (x / 1)) [0 .. 1]
  , pinwheelFanRadialFreqs = [0 .. 7] -- L.map (\x -> x / 8 * pi) [0, 6, 8, 10]
  , pinwheelFanAngularFreqs = [0 .. 7]
  , pinwheelFanTheta = L.map (* (2 * pi)) [0.05,0.1 .. 1]
  }
filterParamsFunc rows cols "PinwheelRing" =
  PinwheelRingParams
  { pinwheelRingRows = rows
  , pinwheelRingCols = cols
  , pinwheelRingGaussianScale = 0.1 * pi
  , pinwheelRingScale = L.map (\x -> 2 ** (x / 4)) [0 .. 0]
  , pinwheelRingRadialFreqs = [5] -- L.map (\x -> x / 8 * pi) [0, 6, 8, 10]
  , pinwheelRingAngularFreqs = [5]
  , pinwheelRingRadius = [5,6,7]
  }
filterParamsFunc rows cols "PinwheelBlob" =
  PinwheelBlobParams
  { pinwheelBlobRows = rows
  , pinwheelBlobCols = cols
  , pinwheelBlobGaussianScale = 2 * pi
  , pinwheelBlobScale = [1]
  , pinwheelBlobFreqs = 0.5 * pi
  , pinwheelBlobOrientation = [0,10 .. 360 - 10]
  , pinwheelBlobThetaShift = [0,32 .. 127 - 32] -- [0,127]
  , pinwheelBlobRadiusShift = [22, 24, 26, 28] -- [0,32]
  }

{-# INLINE getFilterFunc #-}

getFilterFunc
  :: PolarSeparableFilter PolarSeparableFilterExpansion
  -> [VU.Vector (Complex Double)]
getFilterFunc (PolarSeparableFilter _ (FourierMellinFilterExpansion xs)) =
  L.concat xs
getFilterFunc (PolarSeparableFilter _ (GaussianPinwheelFilterExpansion xs)) =
  L.concatMap L.concat xs
getFilterFunc (PolarSeparableFilter _ (PinwheelRingFilterExpansion xs)) =
  L.concatMap L.concat xs
getFilterFunc (PolarSeparableFilter _ (PinwheelFanFilterExpansion xs)) =
  L.concatMap L.concat xs
getFilterFunc (PolarSeparableFilter _ (PinwheelBlobFilterExpansion xs)) =
  L.concat xs
  
{-# INLINE concatStr #-}

concatStr :: [String] -> String
concatStr (x:[]) = x
concatStr (x:xs) = x L.++ "_" L.++ (concatStr xs)

main = do
  xs <- getArgs -- name: FourierMellin, GaussianPinwheel, PinwheelFan,
                -- PinwheelRing, PinwheeBlog
  let rows = 64
      cols = 64
      filterParamsList = L.map (filterParamsFunc rows cols) xs
      filters =
        L.concatMap
          (getFilterFunc . makePolarSeparableFilterExpansion)
          filterParamsList
      imgList =
        L.map
          (IM.arrayToImage .
           listArray ((0, 0), (rows - 1, cols - 1)) . VU.toList)
          filters :: [IM.ComplexImage]
      nameStr = concatStr xs
  createDirectoryIfMissing True "PolarSeparable"
  removePathForcibly ("PolarSeparable" </> nameStr)
  createDirectoryIfMissing True ("PolarSeparable" </> nameStr)
  M.zipWithM_
    (\i img ->
       IM.writeImage
         ("PolarSeparable" </> nameStr </> (printf "%s_%03d.pgm" nameStr i))
         img)
    [(1 :: Int) ..]
    imgList
