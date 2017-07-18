module CV.Utility.Draw where

import           Control.Monad      as M
import           Control.Monad.ST
import           CV.Image
import           Data.Array.Repa    as R
import           Data.Array.ST
import           Data.Array.Unboxed
import           Data.List          as L

data Circle = Circle {circleRadius    :: Int
                     ,circleThickness :: Int}

data Color
  = Yellow
  | Red
  deriving (Show)

{-# INLINE getColor #-}

getColor :: Color -> (Int, Int, Int)
getColor Yellow = (255, 255, 0)
getColor Red    = (255, 0, 0)


drawCircle :: Circle -> Color -> [(Double,Double)] -> ImageRepa -> ImageRepa
drawCircle (Circle radius thickness) color xs (Image depth img)
  | radius > (ceiling . sqrt . fromIntegral $ rows ^ 2 + cols ^ 2) =
    error $ "drawCircle: radius " L.++ show radius L.++ " is too big."
  | depth /= 8 =
    error $ "drawCircle: color depth " L.++ show depth L.++ " is not supported "
  | otherwise =
    let elemList =
          if nf == 3
            then R.toList img
            else let xs = R.toList img
                 in L.concat . L.replicate 3 $ xs
    in Image depth . fromListUnboxed (Z :. (3 :: Int) :. rows :. cols) . elems $
       (runSTUArray $
        (do arr <- newListArray ((0, 0, 0), (2, rows - 1, cols - 1)) elemList
            M.mapM_
              (\(i, j) ->
                  M.mapM_
                    (\n ->
                        M.mapM_
                          (\m ->
                              let theta = 2 * pi / stride * n
                                  idx@(x, y) =
                                    ( round $
                                      i + fromIntegral m +
                                      fromIntegral radius * cos theta
                                    , round $
                                      j + fromIntegral m +
                                      fromIntegral radius * sin theta)
                              in when
                                   (inRange ((0, 0), (rows - 1, cols - 1)) idx)
                                   (do writeArray arr (0, x, y) . fromIntegral $
                                         r
                                       writeArray arr (1, x, y) . fromIntegral $
                                         g
                                       writeArray arr (2, x, y) . fromIntegral $
                                         b))
                          [0,1 .. thickness - 1])
                    [0,1 .. stride - 1])
              xs
            return arr) :: UArray (Int, Int, Int) Double)
  where
    (Z :. nf :. rows :. cols) = extent img
    stride = 4 * pi * fromIntegral r
    (r, g, b) = getColor color
