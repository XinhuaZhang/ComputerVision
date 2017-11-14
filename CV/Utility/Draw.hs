{-# LANGUAGE BangPatterns #-}
module CV.Utility.Draw where

import           Control.Monad       as M
import           CV.Image
import           Control.DeepSeq
import           Data.Array.Repa     as R hiding (Shape)
import           Data.Array.ST
import           Data.Array.Unboxed
import           Data.List           as L

data Shape
  = Circle { circleRadius :: Int
          ,  circleThickness :: Int
          ,  circleCenter :: (Double, Double)}
  | Point { pointCenter :: (Int, Int)}

data Color
  = Yellow
  | Red
  deriving (Show)
  

instance NFData Shape where
  rnf !_ = ()

{-# INLINE getColor #-}

getColor :: Color -> (Double, Double, Double)
getColor Yellow = (255, 255, 0)
getColor Red    = (255, 0, 0)


{-# INLINE drawShape #-}

drawShape
  :: (MArray a e m)
  => a (Int,Int,Int) e -> (e, e, e) -> Shape  -> m ()
drawShape arr (r, g, b) (Circle radius thickness (i, j)) = do
  (_, (_, rows, cols)) <- getBounds arr
  M.mapM_
    (\n ->
        M.mapM_
          (\m ->
              let theta = 2 * pi / stride * n
                  idx@(x, y) =
                    ( round $ i + fromIntegral m + fromIntegral radius * cos theta
                    , round $ j + fromIntegral m + fromIntegral radius * sin theta)
              in when
                   (inRange ((0, 0), (rows, cols)) idx)
                   (do writeArray arr (0, x, y) r
                       writeArray arr (1, x, y) g
                       writeArray arr (2, x, y) b))
          [0,1 .. thickness - 1])
    [0,1 .. stride - 1]
  where
    stride = 4 * pi * fromIntegral radius
drawShape arr (r, g, b) (Point (i, j)) = do
  writeArray arr (0, i, j) r
  writeArray arr (1, i, j) g
  writeArray arr (2, i, j) b


draw :: ImageRepa -> Color -> [Shape] -> ImageRepa
draw (Image depth img) color xs
  | depth == 8 =
    let elemList =
          if nf == 3
            then R.toList img
            else L.concat . L.replicate 3 . R.toList $ img
    in Image depth . fromListUnboxed (Z :. (3 :: Int) :. rows :. cols) . elems $
       (runSTUArray
          (do arr <- newListArray ((0, 0, 0), (2, rows - 1, cols - 1)) elemList
              M.mapM_ (drawShape arr (getColor color)) xs
              return arr) :: UArray (Int, Int, Int) Double)
  | otherwise =
    error $ "drawCircle: color depth " L.++ show depth L.++ " is not supported "
  where
    (Z :. nf :. rows :. cols) = extent img
