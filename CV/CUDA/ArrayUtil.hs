{-# LANGUAGE TypeOperators #-}
module CV.CUDA.ArrayUtil where

import           Data.Array.Accelerate as A
import           Prelude               as P

-- HWD 
crop25D
  :: (Elt a)
  => Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Acc (A.Array DIM3 a)
  -> Acc (A.Array DIM3 a)
crop25D x0 y0 x y nx ny arr
  | x0 < 0 || x0 > (nx - x) || y0 < 0 || y0 > (ny - y) =
    error $
    "AccCropping out of boundary!\n" P.++ show (x0, y0, x, y) P.++ " vs " P.++
    show (nx, ny)
  | otherwise =
    let (Z :. _ :. _ :. len) =
          unlift $ A.shape arr :: Z :. Exp Int :. Exp Int :. Exp Int
    in A.backpermute
         (lift $ Z :. y :. x :. len)
         (\ix ->
             let Z :. j :. i :. k =
                   unlift ix :: Z :. Exp Int :. Exp Int :. Exp Int
             in lift $ Z :. (j + constant y0) :. (i + constant x0) :. k)
         arr

downsample25D :: (Elt a) => Int  -> Acc (A.Array DIM3 a) -> Acc (A.Array DIM3 a)
downsample25D factor arr =
  A.backpermute
    (lift $
     Z :.
     (A.floor $
      (A.fromIntegral ny) / (A.fromIntegral $ lift factor :: Exp Double)) :.
     (A.floor $
      (A.fromIntegral nx) / (A.fromIntegral $ lift factor :: Exp Double)) :.
     nf)
    (\ix ->
        let Z :. j :. i :. k = unlift ix :: Z :. Exp Int :. Exp Int :. Exp Int
        in lift (Z :. (j * lift factor) :. (i * lift factor) :. k))
    arr
  where
    (Z :. ny :. nx :. nf) =
      unlift $ A.shape arr :: Z :. Exp Int :. Exp Int :. Exp Int


rotate3D
  :: Elt e
  => Acc (Array DIM3 e) -> Acc (Array DIM3 e)
rotate3D arr = backpermute (swap1 (A.shape arr)) swap2 arr
  where
    swap1 :: Exp DIM3 -> Exp DIM3
    swap1 ix =
      let Z :. m :. k :. l = unlift ix :: Z :. Exp Int :. Exp Int :. Exp Int
      in lift $ Z :. k :. l :. m
    swap2 :: Exp DIM3 -> Exp DIM3
    swap2 ix =
      let Z :. m :. k :. l = unlift ix :: Z :. Exp Int :. Exp Int :. Exp Int
      in lift $ Z :. l :. m :. k

rotate3D2
  :: Elt e
  => Acc (Array DIM3 e) -> Acc (Array DIM3 e)
rotate3D2 arr = backpermute (swap1 (A.shape arr)) swap2 arr
  where
    swap1 :: Exp DIM3 -> Exp DIM3
    swap1 ix =
      let Z :. m :. k :. l = unlift ix :: Z :. Exp Int :. Exp Int :. Exp Int
      in lift $ Z :. l :. m :. k
    swap2 :: Exp DIM3 -> Exp DIM3
    swap2 ix =
      let Z :. m :. k :. l = unlift ix :: Z :. Exp Int :. Exp Int :. Exp Int
      in lift $ Z :. k :. l :. m
