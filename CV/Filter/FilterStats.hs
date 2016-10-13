{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
module CV.Filter.FilterStats where

import           CV.Utility.Parallel
import           Data.Array.Accelerate              as A
import           Data.Array.Accelerate.CUDA         as A
import           Data.Array.Accelerate.Data.Complex as A
import           Data.Array.Unboxed                 as AU
import           Data.Conduit
import           Data.Conduit.List                  as CL
import           Prelude                            as P

-- Because the filter is default as HWD format and the fold function
-- always fold the rightmost dimension, therefore we need a rotate
-- here.

class CUDAStatistics a  where
  type GPUDataType a :: *
  sink
    :: ParallelParams
    -> [Context]
    -> FilePath
    -> a
    -> Sink (AU.Array (Int, Int, Int) (GPUDataType a)) IO ()

data FilterStats = FilterStats
  { mean :: [Double]
  , var  :: [Double]
  , cov  :: [[Double]]
  } deriving (Show, Read)

rotate3D
  :: Elt e
  => Acc (A.Array DIM3 e) -> Acc (A.Array DIM3 e)
rotate3D arr = backpermute (swap1 (A.shape arr)) swap2 arr
  where
    swap1 :: Exp DIM3 -> Exp DIM3
    swap1 ix =
      let Z :. m :. k :. l = unlift ix :: Z :. Exp Int :. Exp Int :. Exp Int
      in lift $ Z :. l :. m :. k
    swap2 :: Exp DIM3 -> Exp DIM3
    swap2 ix =
      let Z :. m :. k :. l = unlift ix :: Z :. Exp Int :. Exp Int :. Exp Int
      in lift $ Z :. k :. l :. m

filterSum
  :: (Elt a, IsFloating a)
  => Acc (A.Array DIM3 (A.Complex a)) -> Acc (A.Array DIM1 a)
filterSum =
  A.map A.magnitude >-> A.fold (+) (constant 0) >-> A.fold (+) (constant 0)

filterSumSquare
  :: (Elt a, IsFloating a)
  => Acc (A.Array DIM3 (A.Complex a)) -> Acc (A.Array DIM1 a)
filterSumSquare =
  A.map ((^ 2) . A.magnitude) >-> A.fold (+) (constant 0) >->
  A.fold (+) (constant 0)

sampleMean :: (Floating a) => [a] -> Int -> [a]
sampleMean xs n = P.map (/ (P.fromIntegral n)) xs

sampleVar :: (Floating a) => [a] -> [a] -> Int -> [a]
sampleVar s1 s2 n =
  P.zipWith
    (\v1 v2 -> (v2 - v1 ^ 2 / (P.fromIntegral n)) / (P.fromIntegral n - 1))
    s1
    s2

writeFilterStats :: FilePath -> FilterStats -> IO ()
writeFilterStats filePath xs = writeFile filePath $ show xs

readFilterStats :: FilePath -> IO FilterStats
readFilterStats filePath = do
  xs <- readFile filePath
  return $ read xs :: IO FilterStats

