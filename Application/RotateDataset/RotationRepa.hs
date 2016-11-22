{-# LANGUAGE FlexibleContexts #-}
module  Application.RotateDataset.RotationRepa where

import           Control.Monad               as M
import           Control.Monad.IO.Class
import           CV.Array.LabeledArray

import           CV.Utility.Coordinates
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility
import           Data.Array.Repa             as R
import           Data.Conduit
import qualified Data.Conduit.List           as CL
import           Data.List                   as L
import           Data.Vector                 as V
import           Data.Vector.Unboxed         as VU
import           Prelude                     as P
import           System.IO


-- First pading image to be a square image then rotating it
recaleAndRotate2DImageS :: (R.Source s Double)
                        => Int
                        -> Double
                        -> Array s DIM2 Double
                        -> Array U DIM2 Double
recaleAndRotate2DImageS n deg arr =
  computeS $
  fromFunction
    (Z :. n :. n)
    (\(Z :. j :. i) ->
       let (j',i') =
             rotatePixel mat
                         (center,center)
                         (fromIntegral j,fromIntegral i)
       in if inRange j' && inRange i'
             then bicubicInterpolation
                    ds
                    matrixA
                    ((j' - boundaryWith) * ratio,(i' - boundaryWith) * ratio)
             else 0)
  where (Z :. ny :. nx) = extent arr
        m = max ny nx
        innerSize = floor (fromIntegral n / sqrt (2 :: Double))
        boundaryWith = fromIntegral $ div (n - innerSize) 2
        paddedImg = pad [m,m] arr
        ds = computeDerivativeS (computeUnboxedS paddedImg)
        center = fromIntegral n / 2
        mat =
          VU.fromListN 4 $
          P.map (\f -> f (deg2Rad deg))
                [cos,sin,\x -> -(sin x),cos]
        ratio = fromIntegral m / (fromIntegral innerSize - 1)
        matrixA =
          V.fromListN 16 . P.map (VU.fromListN 16) $
          [[1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
          ,[0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0]
          ,[-3,3,0,0,-2,-1,0,0,0,0,0,0,0,0,0,0]
          ,[2,-2,0,0,1,1,0,0,0,0,0,0,0,0,0,0]
          ,[0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0]
          ,[0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0]
          ,[0,0,0,0,0,0,0,0,-3,3,0,0,-2,-1,0,0]
          ,[0,0,0,0,0,0,0,0,2,-2,0,0,1,1,0,0]
          ,[-3,0,3,0,0,0,0,0,-2,0,-1,0,0,0,0,0]
          ,[0,0,0,0,-3,0,3,0,0,0,0,0,-2,0,-1,0]
          ,[9,-9,-9,9,6,3,-6,-3,6,-6,3,-3,4,2,2,1]
          ,[-6,6,6,-6,-3,-3,3,3,-4,4,-2,2,-2,-2,-1,-1]
          ,[2,0,-2,0,0,0,0,0,1,0,1,0,0,0,0,0]
          ,[0,0,0,0,2,0,-2,0,0,0,0,0,1,0,1,0]
          ,[-6,6,6,-6,-4,-2,4,2,-3,3,-3,3,-2,-1,-2,-1]
          ,[4,-4,-4,4,2,2,-2,-2,2,-2,2,-2,1,1,1,1]]
        inRange :: Double -> Bool
        inRange x =
          x >= boundaryWith && x <= (boundaryWith + fromIntegral innerSize - 1)
          

-- recaleAndRotate2DImageP :: (R.Source s Double)
--                         => Int
--                         -> Double
--                         -> Array s DIM2 Double
--                         -> IO (Array U DIM2 Double)
-- recaleAndRotate2DImageP n deg arr = do ds <- computeDerivativeP (computeUnboxedS paddedImg) 
--                                        result <- computeP $
--                                          fromFunction
--                                            (Z :. n :. n)
--                                            (\(Z :. j :. i) ->
--                                               let (j',i') =
--                                                     rotatePixel mat
--                                                                 (center,center)
--                                                                 (fromIntegral j,fromIntegral i)
--                                               in if inRange j' && inRange i'
--                                                     then bicubicInterpolation
--                                                            ds
--                                                            ((j' - boundaryWith) * ratio,(i' - boundaryWith) * ratio)
--                                                     else 0)
--                                        return result
--   where (Z :. ny :. nx) = extent arr
--         m = max ny nx
--         innerSize = floor (fromIntegral n / sqrt (2 :: Double))
--         boundaryWith = fromIntegral $ div (n - innerSize) 2
--         paddedImg = pad [m,m] arr
--         center = fromIntegral n / 2
--         mat =
--           VU.fromListN 4 $
--           P.map (\f -> f (deg2Rad deg))
--                 [cos,sin,\x -> -(sin x),cos]
--         ratio = fromIntegral m / (fromIntegral innerSize - 1)
--         inRange :: Double -> Bool
--         inRange x =
--           x >= boundaryWith && x <= (boundaryWith + fromIntegral innerSize - 1)

rotatePixel :: VU.Vector Double
            -> (Double,Double)
            -> (Double,Double)
            -> (Double,Double)
rotatePixel mat (centerY,centerX) (y,x) = (y3,x3)
  where x1 = x - centerX
        y1 = y - centerY
        (y2,x2) =
          vecMatMult (y1,x1)
                     mat
        x3 = x2 + centerX
        y3 = y2 + centerY

vecMatMult
  :: (Double,Double) -> VU.Vector Double -> (Double,Double)
vecMatMult (x,y) vec = (a * x + c * y,b * x + d * y)
  where a = vec VU.! 0
        b = vec VU.! 1
        c = vec VU.! 2
        d = vec VU.! 3
