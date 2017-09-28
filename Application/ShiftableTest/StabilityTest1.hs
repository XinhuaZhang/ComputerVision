import           CV.IO.ImageIO
import           Data.Array.Repa                                as R
import           Data.Image
import           Data.List                                      as L
import           System.Environment


similarity :: [Double] -> [Double] -> Double
similarity xs ys
  | L.length xs == L.length ys =
    (sqrt (L.sum $ L.zipWith (\x y -> (x - y) ^ (2 :: Int)) xs ys) /
     (fromIntegral . L.length $ xs))
  | otherwise = error "similarity: inputs don't have the same length."
