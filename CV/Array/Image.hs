{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module CV.Array.Image where

import           Codec.Picture
import           Control.Monad                as M
import           Control.Monad.Trans.Resource
import           CV.Image
import           CV.Utility.Coordinates
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility  as RAU
import           Data.Array.Repa              as R
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.Image
import           Data.List                    as L
import           Data.Vector.Unboxed          as VU
import           Data.Word
import           Prelude                      as P
import           System.Random

data ImageTransformationParams = ImageTransformationParams
  { imageTransformationParamsRows :: !Int
  , imageTransformationParamsCols :: !Int
  , rotationAngleParams           :: !Double
  , scaleFactorRange              :: !(Double, Double)   -- the maximum scale is 1
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
normalizeImage upperBound img =
  R.map (\x -> (x - minV) / (maxV - minV) * upperBound) img
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
    !m =
      ceiling
        (sqrt . fromIntegral $ (nx ^ (2 :: Int) + ny ^ (2 :: Int)) :: Double)
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


-- First pading image to be a square image then doing transformation
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
        let -- r = div r' 2
            -- c = div c' 2
            rescaledR = round $ fromIntegral r' * sf
            rescaledC = round $ fromIntegral c' * sf
            ratioR = fromIntegral (m - 1) / fromIntegral (rescaledR - 1)
            ratioC = fromIntegral (m - 1) / fromIntegral (rescaledC - 1)
        in if deg == 0
             then computeS .
                  -- R.map
                  --   (\x ->
                  --       let y = a * x + b
                  --       in if y > 255
                  --            then 255
                  --            else if y < 0
                  --                   then 0
                  --                   else y) .
                  RAU.pad [c', r'] padVal .
                  fromFunction (Z :. rescaledR :. rescaledC) $
                  \(Z :. j :. i) ->
                     bicubicInterpolation
                       ds
                       (minVal, maxVal)
                       (fromIntegral j * ratioR, fromIntegral i * ratioC)
             else computeS .
                  -- R.map
                  --   (\x ->
                  --       let y = a * x + b
                  --       in if y > 255
                  --            then 255
                  --            else if y < 0
                  --                   then 0
                  --                   else y) .
                  RAU.pad [c', r'] padVal .
                  fromFunction (Z :. rescaledR :. rescaledC) $
                  \(Z :. j :. i) ->
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
                                 (j' * ratioR, i' * ratioC))
    transformationList
  where
    minVal = 0
    maxVal = 255
    (Z :. ny :. nx) = extent arr
    m =
      ceiling
        (sqrt . fromIntegral $ (nx ^ (2 :: Int) + ny ^ (2 :: Int)) :: Double)
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
        padTransformGrayImage padVal transformationList . R.slice arr $
        (Z :. i :. All :. All)) $
  [0 .. nf' - 1]
  where
    (Z :. nf' :. _ :. _) = extent arr


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
        in bicubicInterpolation ds valRange (row, col))
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
              exp (deltaR * fromIntegral r) * cos (deltaTheta * fromIntegral t)
            col =
              cCol +
              exp (deltaR * fromIntegral r) * sin (deltaTheta * fromIntegral t)
        in bicubicInterpolation ds valRange (row, col))
  where
    ds = computeDerivativeS . computeS . delay $ arr
    deltaTheta = 2 * pi / fromIntegral ts
    deltaR = polarR / fromIntegral rs

{-# INLINE cartesian2polarImage #-}

cartesian2polarImage :: Int
                     -> Int
                     -> (Double, Double)
                     -> Double
                     -> ImageCoordinates
                     -> ImageCoordinates
cartesian2polarImage ts rs (cRow, cCol) polarR (CartesianImage valueRange arr) =
  PolarImage polarR valueRange .
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
  fromUnboxed (Z :. nf :. ts :. rs) .
  VU.concat .
  L.map
    (\i ->
        toUnboxed .
        computeS .
        cartesian2logpolar2D ts rs (cRow, cCol) logpolarR valueRange . R.slice arr $
        (Z :. i :. All :. All)) $
   [0 .. nf - 1]
  where
    (Z :. nf :. _ :. _) = extent arr
cartesian2logpolarImage _ _ _ _ _ =
  error "cartesian2logpolarImage: input is not a CartesianImage."  

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
