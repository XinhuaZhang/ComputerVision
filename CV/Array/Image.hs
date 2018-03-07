{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module CV.Array.Image where

import           Codec.Picture
import           Control.Monad                as M
import           Control.Monad.ST
import           Control.Monad.Trans.Resource
import           CV.Image
import           CV.Utility.Coordinates
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility  as RAU
import           Data.Array.Repa              as R
import           Data.Array.ST
import qualified Data.Array.IArray as IA
import qualified Data.Array.Unboxed as UA
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.Image
import           Data.List                    as L
import           Data.Vector.Unboxed          as VU
import           Data.Word
import           Prelude                      as P
import           System.Random
import Text.Printf

data ImageTransformationParams = ImageTransformationParams
  { imageTransformationParamsRows :: !Int
  , imageTransformationParamsCols :: !Int
  , rotationAngleParams           :: !Double
  , scaleFactorRange              :: !(Double, Double)   -- if the inputSize * scale is greater than outputSize, it crops.
  , contrastFactorARange          :: !(Double, Double)
  , contrastFactorBRange          :: !(Double, Double)
  } deriving (Read, Show)


data ImageTransformation = ImageTransformation
  { imageTransformationRows :: !Int
  , imageTransformationCols :: !Int
  , rotationAngle           :: !Double
  , scaleFactor             :: !Double
  , contrastFactorA         :: !Double
  , contrastFactorB         :: !Double
  } deriving (Read, Show)


generateImageTransformation :: ImageTransformationParams
                            -> IO [ImageTransformation]
generateImageTransformation (ImageTransformationParams r c deg sfRange aRange bRange) =
  M.mapM
    (\radDeg -> do
       sf <- randomRIO sfRange
       a <- randomRIO aRange
       b <- randomRIO bRange
       return $! ImageTransformation r c radDeg sf a b)
    radAngle
  where
    radAngle =
      if deg == 0
        then [0]
        else L.map deg2Rad [0,deg .. 360 - deg]


grayImage2Array
  :: GrayImage -> Array U DIM3 Double
grayImage2Array img = fromListUnboxed (Z :. 1 :. ny :. nx) $ pixelList img
  where (ny,nx) = dimensions img

colorImage2Array
  :: ColorImage -> Array U DIM3 Double
colorImage2Array img =
  fromListUnboxed (Z :. 3 :. ny :. nx) $
  P.concatMap pixelList
              [r,g,b]
  where (ny,nx) = dimensions img
        (r,g,b) = colorImageToRGB img

normalizeImage
  :: Double -> Array U DIM3 Double -> Array D DIM3 Double
normalizeImage upperBound img
  | (VU.all (== 0) . toUnboxed $ img) = delay img
  | otherwise = R.map (\x -> (x - minV) / (maxV - minV) * upperBound) img
  where
    maxV = foldAllS max (fromIntegral (minBound :: Int)) img
    minV = foldAllS min (fromIntegral (maxBound :: Word)) img

plotImage :: FilePath -> Array U DIM3 Double -> IO ()
plotImage filePath img = do
  let Z :. nfp' :. nyp' :. nxp' = extent img
      normalizedImg =
        computeUnboxedS $
        normalizeImage (P.fromIntegral (maxBound :: Pixel8)) img
      w =
        case nfp' of
          1 ->
            ImageY8 $
            generateImage
              (\i j ->
                 let v =
                       fromIntegral . round $ normalizedImg R.! (Z :. 0 :. j :. i)
                 in v)
              nxp'
              nyp'
          3 ->
            ImageRGB8 $
            generateImage
              (\i j ->
                 let r =
                       fromIntegral . round $ normalizedImg R.! (Z :. 0 :. j :. i)
                     g =
                       fromIntegral . round $ normalizedImg R.! (Z :. 1 :. j :. i)
                     b =
                       fromIntegral . round $ normalizedImg R.! (Z :. 2 :. j :. i)
                 in PixelRGB8 r g b)
              nxp'
              nyp'
          _ ->
            error $
            "Image is neither a gray image nor a color image. There are " P.++
            show nfp' P.++
            " channels."
  savePngImage filePath w


rgb2ColorOpponency
  :: (R.Source s Double)
  => Array s DIM3 Double -> Array D DIM3 Double
rgb2ColorOpponency arr
  | nc == 3 =
    fromFunction
      (extent arr)
      (\(Z :. k :. j :. i) ->
         let r = arr R.! (Z :. 0 :. j :. i)
             g = arr R.! (Z :. 1 :. j :. i)
             b = arr R.! (Z :. 2 :. j :. i)
             y = (r + g) / 2
         in case k of
              0 ->
                if r + g == 0
                  then 0
                  else (r - g) / (r + g)
              1 ->
                if b + y == 0
                  then 0
                  else (b - y) / (b + y)
              2 -> r + g + b
              _ -> error "rgb2ColorOpponency: image channel error.")
  | otherwise =
    error $
    "rgb2ColorOpponency: the number of channels is not 3 but " L.++ show nc
  where
    (Z :. nc :. _ :. _) = extent arr

-- Set the maximum value of the maximum size, the ratio is intact.
{-# INLINE resize2DImageS #-}   

resize2DImageS
  :: (R.Source s Double)
  => Int -> Array s DIM2 Double -> Array U DIM2 Double
resize2DImageS n arr =
  computeS . fromFunction (Z :. newNy :. newNx) $
  \(Z :. j :. i) ->
     bicubicInterpolation
       ds
       (minVal, maxVal)
       (fromIntegral j * ratioY, fromIntegral i * ratioX)
  where
    !(Z :. ny :. nx) = extent arr
    !newNy =
      if ny >= nx
        then n
        else round
               (fromIntegral n * fromIntegral ny / fromIntegral nx :: Double)
    !newNx =
      if ny >= nx
        then round
               (fromIntegral n * fromIntegral nx / fromIntegral ny :: Double)
        else n
    !ratioX = fromIntegral (nx - 1) / fromIntegral (newNx - 1)
    !ratioY = fromIntegral (ny - 1) / fromIntegral (newNy - 1)
    !minVal = foldAllS min (fromIntegral (maxBound :: Word32)) arr
    !maxVal = 255 -- foldAllS max (fromIntegral (minBound :: Int)) arr
    !ds = computeDerivativeS . computeUnboxedS . delay $ arr

-- Set the maximum value of the maximum size, the ratio is intact.
resizeImageConduit
  :: (R.Source s Double)
  => ParallelParams
  -> Int
  -> Conduit (R.Array s DIM3 Double) (ResourceT IO) (R.Array U DIM3 Double)
resizeImageConduit parallelParams n = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rseq
                (\x ->
                    let (Z :. nf' :. _ :. _) = extent x
                        (Z :. ny' :. nx') = extent . L.head $ zs
                        zs =
                          L.map
                            (\i ->
                                resize2DImageS n . R.slice x $
                                (Z :. i :. All :. All))
                            [0 .. nf' - 1]
                        arr = fromUnboxed (Z :. nf' :. ny' :. nx') .
                              VU.concat . L.map toUnboxed $
                              zs
                    in deepSeqArray arr arr)
                xs
        sourceList ys
        resizeImageConduit parallelParams n)


cropResizeImageConduit
  :: (R.Source s Double)
  => ParallelParams
  -> Int
  -> Conduit (R.Array s DIM3 Double) (ResourceT IO) (R.Array U DIM3 Double)
cropResizeImageConduit parallelParams n = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rseq
                (\x ->
                   let (Z :. nf' :. ny' :. nx') = extent x
                       diff = div (abs $ ny' - nx') 2
                       y =
                         if ny' == nx'
                           then delay x
                           else if ny' > nx'
                                  then RAU.crop [0, diff, 0] [nx', nx', nf'] x
                                  else RAU.crop [diff, 0, 0] [ny', ny', nf'] x
                       zs =
                         L.map
                           (\i ->
                              resize2DImageS n . R.slice y $
                              (Z :. i :. All :. All))
                           [0 .. nf' - 1]
                       arr =
                         fromUnboxed (Z :. nf' :. n :. n) .
                         VU.concat . L.map toUnboxed $
                         zs
                   in deepSeqArray arr arr)
                xs
        sourceList ys
        cropResizeImageConduit parallelParams n)

padResizeImageConduit
  :: (R.Source s Double)
  => ParallelParams
  -> Int
  -> Double
  -> Conduit (R.Array s DIM3 Double) (ResourceT IO) (R.Array U DIM3 Double)
padResizeImageConduit parallelParams n padVal = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rseq
                (\x ->
                   let (Z :. nf' :. ny' :. nx') = extent x
                       maxSize = max ny' nx'
                       y = RAU.pad [maxSize, maxSize, nf'] padVal x
                       zs =
                         L.map
                           (\i ->
                              resize2DImageS n . R.slice y $
                              (Z :. i :. All :. All))
                           [0 .. nf' - 1]
                       arr =
                         fromUnboxed (Z :. nf' :. n :. n) .
                         VU.concat . L.map toUnboxed $
                         zs
                   in deepSeqArray arr arr)
                xs
        sourceList ys
        padResizeImageConduit parallelParams n padVal)


{-# INLINE rotatePixel #-}

rotatePixel :: VU.Vector Double
            -> (Double, Double)
            -> (Double, Double)
            -> (Double, Double)
rotatePixel mat (centerY, centerX) (y, x) = (y3, x3)
  where
    x1 = x - centerX
    y1 = y - centerY
    (y2, x2) = vecMatMult (y1, x1) mat
    x3 = x2 + centerX
    y3 = y2 + centerY

{-# INLINE vecMatMult #-}

vecMatMult :: (Double, Double) -> VU.Vector Double -> (Double, Double)
vecMatMult (x, y) vec = (a * x + c * y, b * x + d * y)
  where
    a = vec VU.! 0
    b = vec VU.! 1
    c = vec VU.! 2
    d = vec VU.! 3

-- First pading image to be a square image then rotating it
padResizeRotate2DImageS
  :: (R.Source s Double)
  => Int -> [Double] -> Array s DIM2 Double -> [Array U DIM2 Double]
padResizeRotate2DImageS n degs arr =
  parMap
    rseq
    (\deg ->
       computeS $ --pad [n+32, n+32] $
       fromFunction
         (Z :. n :. n)
         (\(Z :. j :. i) ->
            let (j', i') =
                  rotatePixel
                    (VU.fromListN 4 $
                     P.map
                       (\f -> f (deg2Rad deg))
                       [cos, sin, \x -> -(sin x), cos])
                    (center, center)
                    (fromIntegral j, fromIntegral i)
            in if j' < 0 ||
                  j' > (fromIntegral n - 1) ||
                  i' < 0 || i' > (fromIntegral n - 1)
                 then 0
                 else bicubicInterpolation
                        ds
                        (minVal, maxVal)
                        (j' * ratio, i' * ratio)))
    degs
  where
    !minVal = 0 -- foldAllS min (fromIntegral (maxBound :: Word64)) arr
    !maxVal = 255 -- foldAllS max (fromIntegral (minBound :: Int)) arr
    !(Z :. ny :. nx) = extent arr
    !m = max nx ny
      -- ceiling
      --   (sqrt . fromIntegral $ (nx ^ (2 :: Int) + ny ^ (2 :: Int)) :: Double)
    !paddedImg = RAU.pad [m, m] 0 arr
    !ds = computeDerivativeS (computeUnboxedS paddedImg)
    !center = fromIntegral (n - 1) / 2
    !ratio = fromIntegral (m - 1) / fromIntegral (n - 1)



padResizeRotateImageConduit
  :: (R.Source s Double)
  => ParallelParams
  -> Int
  -> Double
  -> Conduit (R.Array s DIM3 Double) (ResourceT IO) (R.Array U DIM3 Double)
padResizeRotateImageConduit parallelParams n deg = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rseq
                (\arr ->
                   let (Z :. nf :. _ny :. _nx) = extent arr
                   in L.map
                        (\x ->
                           let arr' =
                                 fromUnboxed (Z :. nf :. n :. n) .
                                 VU.concat . L.map R.toUnboxed $
                                 x
                           in deepSeqArray arr' arr') .
                      L.transpose .
                      L.map
                        (\i ->
                           padResizeRotate2DImageS n degs $
                           R.slice arr (Z :. i :. All :. All)) $
                      [0 .. nf - 1])
                xs
        sourceList . P.concat $ ys
        padResizeRotateImageConduit parallelParams n deg)
  where
    !len =
      if deg == 0
        then 1
        else round (360 / deg) :: Int
    !degs = L.map (* deg) [0 .. fromIntegral len - 1]


-- First pading image to be a square image then doing transformation, then padding it to the target size
{-# INLINE padTransformGrayImage #-}   

padTransformGrayImage
  :: (R.Source s Double)
  => Double
  -> [ImageTransformation]
  -> R.Array s DIM2 Double
  -> [R.Array U DIM2 Double]
padTransformGrayImage padVal transformationList arr =
  L.map
    (\(ImageTransformation r' c' deg sf a b) ->
       let rescaledR = round $ fromIntegral ny * sf
           rescaledC = round $ fromIntegral nx * sf
           ratioR = fromIntegral (m - 1) / fromIntegral (rescaledR - 1)
           ratioC = fromIntegral (m - 1) / fromIntegral (rescaledC - 1)
           (startR, lenR) =
             if rescaledR > r'
               then (div (rescaledR - r') 2, r')
               else (0, rescaledR)
           (startC, lenC) =
             if rescaledC > c'
               then (div (rescaledC - c') 2, c')
               else (0, rescaledC)
           rescaledArr =
             if deg == 0
               then if sf == 1
                      then paddedImg
                      else fromFunction (Z :. rescaledR :. rescaledC) $ \(Z :. j :. i) ->
                             bicubicInterpolation
                               ds
                               (minVal, maxVal)
                               ( fromIntegral j * ratioR
                               , fromIntegral i * ratioC)
               else fromFunction (Z :. rescaledR :. rescaledC) $ \(Z :. j :. i) ->
                      let (j', i') =
                            rotatePixel
                              (VU.fromListN 4 $
                               P.map
                                 (\f -> f deg)
                                 [cos, sin, \x -> -(sin x), cos])
                              ( fromIntegral $ div rescaledR 2
                              , fromIntegral $ div rescaledC 2)
                              (fromIntegral j, fromIntegral i)
                      in if j' < 0 ||
                            j' > (fromIntegral rescaledR - 1) ||
                            i' < 0 || i' > (fromIntegral rescaledC - 1)
                           then padVal
                           else bicubicInterpolation
                                  ds
                                  (minVal, maxVal)
                                  (j' * ratioR, i' * ratioC)
       in computeS . RAU.pad [c', r'] padVal $
          if rescaledC > c' || rescaledR > r'
            then RAU.crop [startC, startR] [lenC, lenR] $ rescaledArr
            else rescaledArr)
    transformationList
  where
    minVal = 0
    maxVal = 255
    (Z :. ny :. nx) = extent arr
    m = max nx ny
      -- ceiling
      --   (sqrt . fromIntegral $ (nx ^ (2 :: Int) + ny ^ (2 :: Int)) :: Double)
    paddedImg = RAU.pad [m, m] padVal arr
    ds = computeDerivativeS (computeUnboxedS paddedImg)


{-# INLINE padTransformImage #-}   

padTransformImage
  :: (R.Source s Double)
  => Double
  -> [ImageTransformation]
  -> R.Array s DIM3 Double
  -> [R.Array U DIM3 Double]
padTransformImage padVal transformationList arr =
  L.zipWith
    (\transfomation arrs ->
       fromUnboxed
         (Z :. nf' :. imageTransformationRows transfomation :.
          imageTransformationCols transfomation) .
       VU.concat . L.map toUnboxed $
       arrs)
    transformationList .
  L.transpose .
  L.map
    (\i ->
       padTransformGrayImage (arr R.! (Z :. i :. 0 :. 0)) transformationList .
       R.slice arr $
       (Z :. i :. All :. All)) $
  [0 .. nf' - 1]
  where
    (Z :. nf' :. _ :. _) = extent arr
    

-- First padding image to be a square image then doing transformation,
-- The output's size may vary for different scales.

{-# INLINE transformGrayImage #-}   

transformGrayImage
  :: (R.Source s Double)
  => Double
  -> [ImageTransformation]
  -> R.Array s DIM2 Double
  -> [R.Array U DIM2 Double]
transformGrayImage padVal transformationList arr =
  L.map
    (\(ImageTransformation r' c' deg sf a b) ->
       let rescaledR = round $ fromIntegral ny * sf
           rescaledC = round $ fromIntegral nx * sf
           ratioR = fromIntegral (m - 1) / fromIntegral (rescaledR - 1)
           ratioC = fromIntegral (m - 1) / fromIntegral (rescaledC - 1)
           (startR, lenR) =
             if rescaledR > r'
               then (div (rescaledR - r') 2, r')
               else (0, rescaledR)
           (startC, lenC) =
             if rescaledC > c'
               then (div (rescaledC - c') 2, c')
               else (0, rescaledC)
           rescaledArr =
             if deg == 0
               then fromFunction (Z :. rescaledR :. rescaledC) $ \(Z :. j :. i) ->
                      bicubicInterpolation
                        ds
                        (minVal, maxVal)
                        (fromIntegral j * ratioR, fromIntegral i * ratioC)
               else fromFunction (Z :. rescaledR :. rescaledC) $ \(Z :. j :. i) ->
                      let (j', i') =
                            rotatePixel
                              (VU.fromListN 4 $
                               P.map
                                 (\f -> f deg)
                                 [cos, sin, \x -> -(sin x), cos])
                              ( fromIntegral $ div rescaledR 2
                              , fromIntegral $ div rescaledC 2)
                              (fromIntegral j, fromIntegral i)
                      in if j' < 0 ||
                            j' > (fromIntegral rescaledR - 1) ||
                            i' < 0 || i' > (fromIntegral rescaledC - 1)
                           then padVal
                           else bicubicInterpolation
                                  ds
                                  (minVal, maxVal)
                                  (j' * ratioR, i' * ratioC)
       in computeS $
          if rescaledC > c' || rescaledR > r'
            then RAU.crop [startC, startR] [lenC, lenR] $ rescaledArr
            else rescaledArr)
    transformationList
  where
    minVal = 0
    maxVal = 255
    (Z :. ny :. nx) = extent arr
    m = max nx ny
    paddedImg = RAU.pad [m, m] padVal arr
    ds = computeDerivativeS (computeUnboxedS paddedImg)


{-# INLINE transformImage #-}   

transformImage
  :: (R.Source s Double)
  => Double
  -> [ImageTransformation]
  -> R.Array s DIM3 Double
  -> [R.Array U DIM3 Double]
transformImage padVal transformationList arr =
  L.zipWith
    (\transfomation arrs ->
       fromUnboxed
         (Z :. nf' :. round (scaleFactor transfomation * fromIntegral rows) :.
          round (scaleFactor transfomation * fromIntegral cols)) .
       VU.concat . L.map toUnboxed $
       arrs)
    transformationList .
  L.transpose .
  L.map
    (\i ->
       transformGrayImage padVal transformationList . R.slice arr $
       (Z :. i :. All :. All)) $
  [0 .. nf' - 1]
  where
    (Z :. nf' :. rows :. cols) = extent arr


{-# INLINE cartesian2polar2D #-}

cartesian2polar2D
  :: (R.Source s Double)
  => Int
  -> Int
  -> (Double, Double)
  -> Double
  -> (Double, Double)
  -> R.Array s DIM2 Double
  -> R.Array D DIM2 Double
cartesian2polar2D ts rs (cRow, cCol) polarR valRange arr =
  fromFunction
    (Z :. ts :. rs)
    (\(Z :. t :. r) ->
       let row =
             cRow +
             (deltaR * fromIntegral r) * cos (deltaTheta * fromIntegral t)
           col =
             cCol +
             (deltaR * fromIntegral r) * sin (deltaTheta * fromIntegral t)
       in (bicubicInterpolation ds valRange (row, col)) / (fromIntegral ts) * 2 *
          pi *
          deltaR *
          fromIntegral r)
  where
    ds = computeDerivativeS . computeS . delay $ arr
    deltaTheta = 2 * pi / fromIntegral ts
    deltaR = polarR / fromIntegral rs

{-# INLINE cartesian2logpolar2D #-}

cartesian2logpolar2D
  :: (R.Source s Double)
  => Int
  -> Int
  -> (Double, Double)
  -> Double
  -> (Double, Double)
  -> R.Array s DIM2 Double
  -> R.Array D DIM2 Double
cartesian2logpolar2D ts rs (cRow, cCol) polarR valRange arr =
  fromFunction
    (Z :. ts :. rs)
    (\(Z :. t :. r) ->
       let row =
             cRow +
             (exp (deltaR * fromIntegral r)) * cos (deltaTheta * fromIntegral t)
           col =
             cCol +
             (exp (deltaR * fromIntegral r)) * sin (deltaTheta * fromIntegral t)
       in bicubicInterpolation ds valRange (row, col)
          -- (bicubicInterpolation ds valRange (row, col)) *
          -- ((1 / (fromIntegral ts) * 2 * pi * (exp  (fromIntegral r * deltaR))) ^
          --  (1 :: Int))
    )
  where
    ds = computeDerivativeS . computeS . delay $ arr
    deltaTheta = 2 * pi / fromIntegral ts
    deltaR = (log polarR) / fromIntegral rs

{-# INLINE cartesian2polarImage #-}

cartesian2polarImage :: Int
                     -> Int
                     -> (Double, Double)
                     -> Double
                     -> ImageCoordinates
                     -> ImageCoordinates
cartesian2polarImage ts rs (cRow, cCol) polarR (CartesianImage valueRange arr) =
  PolarImage polarR valueRange .
  -- computeS .
  -- normalizeImage 255 .
  fromUnboxed (Z :. nf :. ts :. rs) .
  VU.concat .
  L.map
    (\i ->
       toUnboxed .
       computeS .
       cartesian2polar2D ts rs (cRow, cCol) polarR valueRange . R.slice arr $
       (Z :. i :. All :. All)) $
  [0 .. nf - 1]
  where
    (Z :. nf :. _ :. _) = extent arr
cartesian2polarImage _ _ _ _ _ =
  error "cartesian2polarImage: input is not a CartesianImage."

{-# INLINE cartesian2logpolarImage #-}

cartesian2logpolarImage :: Int
                        -> Int
                        -> (Double, Double)
                        -> Double
                        -> ImageCoordinates
                        -> ImageCoordinates
cartesian2logpolarImage ts rs (cRow, cCol) logpolarR (CartesianImage valueRange arr) =
  LogpolarImage logpolarR valueRange .
  computeS .
  normalizeImage 255 .
  fromUnboxed (Z :. nf :. ts :. rs) .
  VU.concat .
  L.map
    (\i ->
       toUnboxed .
       computeS .
       cartesian2logpolar2D ts rs (cRow, cCol) logpolarR valueRange .
       R.slice arr $
       (Z :. i :. All :. All)) $
  [0 .. nf - 1]
  where
    (Z :. nf :. _ :. _) = extent arr
cartesian2logpolarImage _ _ _ _ _ =
  error "cartesian2logpolarImage: input is not a CartesianImage."  
  
{-# INLINE logpolar2Cartesian2D #-}

logpolar2Cartesian2D
  :: (R.Source s Double)
  => Int
  -> Int
  -> Double
  -> (Double, Double)
  -> (Double, Double)
  -> R.Array s DIM2 Double
  -> R.Array D DIM2 Double
logpolar2Cartesian2D rows cols polarR valRange (centerR, centerC) arr =
  let ds = computeDerivativeS . computeS . delay $ arr
      (Z :. ts :. rs) = extent arr
      deltaTheta = 2 * pi / fromIntegral ts
      deltaR = (log polarR) / fromIntegral rs
  in fromFunction
       (Z :. rows :. cols)
       (\(Z :. r :. c) ->
          let x = (fromIntegral r) - centerR
              y = (fromIntegral c) - centerC
              theta = angleFunctionRad x y
              radius = log . sqrt $ x ^ (2 :: Int) + y ^ (2 :: Int)
          in bicubicInterpolation
               ds
               valRange
               (theta / deltaTheta, radius / deltaR))
               
{-# INLINE logpolar2CartesianImage #-}

logpolar2CartesianImage :: Int
                        -> Int
                        -> (Double, Double)
                        -> ImageCoordinates
                        -> ImageCoordinates
logpolar2CartesianImage rows cols center (LogpolarImage polarR valRange arr) =
  let (Z :. nf :. _ :. _) = extent arr
  in CartesianImage valRange . fromUnboxed (Z :. nf :. rows :. cols) . VU.concat $
     [ toUnboxed .
     computeS .
     logpolar2Cartesian2D rows cols polarR valRange center . R.slice arr $
     (Z :. i :. All :. All)
     | i <- [0 .. nf - 1]
     ]
logpolar2CartesianImage _ _ _ _ =
  error "logpolar2Cartesian2D: image type is not LogpolarImage."

{-# INLINE shiftGrayImage #-}

shiftGrayImage
  :: (R.Source s Double)
  => Int -> Int -> Double -> R.Array s DIM2 Double -> R.Array U DIM2 Double
shiftGrayImage rowShift colShift padVal arr
  | abs rowShift >= rows || abs colShift >= cols =
    error $
    "shiftGrayImage: shift values are greater than sizes.\n" L.++
    show (rows, cols) L.++
    " vs " L.++
    show (rowShift, colShift)
  | otherwise =
    computeUnboxedS .
    RAU.pad [cols, rows] padVal .
    RAU.crop
      (L.map startPointFunc [colShift, rowShift])
      [cols - abs colShift, rows - abs rowShift] $
    arr
  where
    (Z :. rows :. cols) = extent arr
    startPointFunc xShift =
      if xShift > 0
        then 0
        else -xShift


shiftImage
  :: (R.Source s Double)
  => Int -> Int -> Double -> R.Array s DIM3 Double -> R.Array U DIM3 Double
shiftImage rowShift colShift padVal arr =
  fromUnboxed (extent arr) .
  VU.concat .
  L.map
    (\i ->
        toUnboxed . shiftGrayImage rowShift colShift padVal . R.slice arr $
        (Z :. i :. All :. All)) $
  [0 .. nf - 1]
  where
    (Z :. nf :. _ :. _) = extent arr


{-# INLINE array2RepaArray #-}

array2RepaArray
  :: (IA.IArray arr Double)
  => arr (Int, Int, Int) Double -> R.Array U DIM3 Double
array2RepaArray array =
  let ((nfLB, rowsLB, colsLB), (nfUB, rowsUB, colsUB)) = IA.bounds array
  in fromListUnboxed (Z :. nfUB - nfLB + 1 :. rowsUB - rowsLB + 1 :. colsUB - colsLB + 1) . IA.elems $ array


insertPatch
  :: (R.Source s1 Double, R.Source s2 Double)
  => R.Array s1 DIM3 Double
  -> [((Int, Int), R.Array s2 DIM3 Double)]        -- (originIdx (not center), arr)
  -> R.Array U DIM3 Double
insertPatch arr patches
  | nf1 /= nf2 || rows2 > rows1 || cols2 > cols1 =
    error $
    printf
      "insertPatch:\n(rows,cols,nf)\n(%d,%d,%d)\n(%d,%d,%d)\n"
      rows1
      cols1
      nf1
      rows2
      cols2
      nf2
  | otherwise =
    array2RepaArray $
    runSTUArray $ do
      img <-
        newListArray ((0, 0, 0), (nf1 - 1, rows1 - 1, cols1 - 1)) (R.toList arr)
      M.mapM_
        (\((i0, j0), patch) ->
           M.mapM_
             (\(i, j) ->
                M.mapM_
                  (\k ->
                     let x = patch R.! (Z :. k :. i :. j)
                     in unless
                          (x == 0)
                           -- if (i+i0) >= rows1 || (j+j0) >= cols1
                           --    then error $ printf "insertPatch: out of boundary\n(i0,j0): (%d,%d)\n(i,j): (%d,%d)\n(rows1,cols1): (%d,%d)\n" i0 j0 i j rows1 cols1
                           --    else
                          (writeArray img (k, (i + i0), (j + j0)) x)
                        -- error $ printf "insertPatch: out of boundary\n(i0,j0): (%d,%d)\n(i,j): (%d,%d)\n(rows1,cols1): (%d,%d)\n(rows1,cols1): (%d,%d)\n" i0 j0 i j rows1 cols1 rows2 cols2
                   )
                  [0 .. nf2 - 1])
             [(i, j) | i <- [0 .. rows2 - 1], j <- [0 .. cols2 - 1]])
        patches
      return img
  where
    (Z :. nf1 :. rows1 :. cols1) = extent arr
    (Z :. nf2 :. rows2 :. cols2) = extent . snd . L.head $ patches
