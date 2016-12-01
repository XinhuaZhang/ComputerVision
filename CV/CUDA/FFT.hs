{-# LANGUAGE CPP                      #-}
{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE ViewPatterns             #-}

-- This module considers a filter as a 2D array.  Several filters
-- consist of a 3D array. When applying fft, each layer is applied
-- individually. This is different from 3D-fft.
module CV.CUDA.FFT
  ( ArrayFormat(..)
  , fft25D'
  , centre25D
  , coveriance25D
  , concatComplex
  , gaussianNormalize
  ) where

import           CV.CUDA.ArrayUtil
import           Data.Array.Accelerate              as A
import           Data.Array.Accelerate.Array.Sugar  as S (EltRepr, shape,
                                                          shapeToList)
import           Data.Array.Accelerate.Array.Sugar  (showShape)
import           Data.Array.Accelerate.CUDA.Foreign
import           Data.Array.Accelerate.Data.Complex as A
import           Data.Array.Accelerate.Math.FFT     as A
import           Data.Array.Accelerate.Type
import           Data.Bits
import           Data.Functor
import qualified Foreign.CUDA.Driver                as CUDA hiding (free)
import qualified Foreign.CUDA.Driver                as CUDA
import           Foreign.CUDA.FFT
import qualified Foreign.CUDA.Types                 as CUDA
import           Prelude                            as P
import           System.IO.Unsafe
import           System.Mem.Weak

-- D: nf, W:nx, H:ny
data ArrayFormat
  = DHW
  | HWD
  deriving (Show, Eq)

-- the fft' function will not be used!!! What matters is the shape of the input array
fft25D'
  :: forall e.
     (Elt e, IsFloating e)
  => Mode
  -> Int -- ^ width
  -> Int -- ^ height
  -> Int -- ^ depth
  -> ArrayFormat
  -> Acc (Array DIM3 (Complex e))
  -> Acc (Array DIM3 (Complex e))
fft25D' mode width height depth format arr =
  let sign = signOfMode mode :: e
      scale = P.fromIntegral (width * height * depth)
      sh =
        case format of
          DHW -> Z :. depth :. height :. width
          HWD -> Z :. height :. width :. depth
      arr' = cudaFFT mode sh fft' arr
      fft' a =
        rotate3D2 . A.fft sign (Z :. width :. depth) height >-> rotate3D .
        A.fft sign (Z :. depth :. height) width $
        a
  in if P.not (isPow2 width && isPow2 height)
       then error $
            unlines
              [ "Data.Array.Accelerate.FFT: fft25D"
              , "  Array dimensions must be powers of two, but are: " P.++
                showShape (Z :. depth :. height :. width)
              ]
       else case mode of
              Inverse -> A.map (/ scale) arr'
              _       -> arr'

centre25D
  :: (Elt e, IsFloating e)
  => ArrayFormat -> Acc (Array DIM3 (Complex e)) -> Acc (Array DIM3 (Complex e))
centre25D DHW arr =
  A.generate
    (A.shape arr)
    (\ix ->
        let Z :. _ :. y :. x = unlift ix :: Z :. Exp Int :. Exp Int :. Exp Int
        in lift (((-1) ** A.fromIntegral (y + x)) :+ A.constant 0) * arr ! ix)
centre25D HWD arr =
  A.generate
    (A.shape arr)
    (\ix ->
        let Z :. y :. x :. _ = unlift ix :: Z :. Exp Int :. Exp Int :. Exp Int
        in lift (((-1) ** A.fromIntegral (y + x)) :+ A.constant 0) * arr ! ix)

coveriance25D :: Acc (A.Array DIM3 Double) -> Acc (A.Array DIM2 Double)
coveriance25D arr =
  let Z :. nf :. ny :. nx =
        unlift $ A.shape arr :: Z :. Exp Int :. Exp Int :. Exp Int
  in A.fold
       (+)
       (constant 0)
       (generate
          (lift $ Z :. nf :. nf :. (nx * ny))
          (\ix ->
              let Z :. j :. i :. k =
                    unlift ix :: Z :. Exp Int :. Exp Int :. Exp Int
                  z =
                    A.floor
                      ((A.fromIntegral k :: Exp Double) /
                       (A.fromIntegral nx :: Exp Double))
                  v1 = arr ! lift (Z :. i :. z :. k - (z * nx))
                  v2 = arr ! lift (Z :. j :. z :. k - (z * nx))
              in v1 * v2))

concatComplex :: Acc (A.Array DIM3 (Complex Double))
              -> Acc (A.Array DIM3 Double)
concatComplex arr =
  generate
    (lift (Z :. 2 * nf :. ny :. nx))
    (\ix ->
        let Z :. k :. i :. j = unlift ix :: Z :. Exp Int :. Exp Int :. Exp Int
        in cond
             (k A.<* nf)
             (real $ arr ! lift (Z :. k :. i :. j))
             (imag $ arr ! lift (Z :. (k - nf) :. i :. j)))
  where
    Z :. nf :. ny :. nx =
      unlift $ A.shape arr :: Z :. Exp Int :. Exp Int :. Exp Int

gaussianNormalize :: Acc (A.Array DIM3 Double) -> Acc (A.Array DIM3 Double)
gaussianNormalize arr =
  let Z :. k :. j :. i =
        unlift $ A.shape arr :: Z :. Exp Int :. Exp Int :. Exp Int
  in generate
       (lift (Z :. (k - 1) :. j :. i))
       (\ix ->
           let Z :. z :. y :. x = unlift ix :: Z :. Exp Int :. Exp Int :. Exp Int
               a = arr ! lift (Z :. z + 1 :. y :. x)
               b = arr ! lift (Z :. constant 0 :. y :. x)
           in a / b)

{- Copy from module Data.Array.Accelerate.Math.FFT-}
isPow2 :: Int -> Bool
isPow2 x = x .&. (x - 1) == 0

signOfMode
  :: Num a
  => Mode -> a
signOfMode m =
  case m of
    Forward -> -1
    Reverse -> 1
    Inverse -> 1

cudaFFT
  :: forall e sh.
     (Shape sh, Elt e, IsFloating e)
  => Mode
  -> sh
  -> (Acc (Array sh (Complex e)) -> Acc (Array sh (Complex e)))
  -> Acc (Array sh (Complex e))
  -> Acc (Array sh (Complex e))
cudaFFT mode sh = cudaFFT'
-- Plan the FFT.  Doing this in unsafePerformIO so it is not
-- reperformed every time the AST is evaluated.
--
  where
    hndl =
      unsafePerformIO $
      do plan <-
           case shapeToList sh of
             [width] -> plan1D width types 1
             [width, height] -> plan2D height width types
             [width, height, depth] -> plan3D depth height width types
             _ ->
               error
                 "Accelerate-fft cannot use CUFFT for arrays of dimensions higher than 3"
         addFinalizer plan (destroy plan)
         return plan
    types =
      case (floatingType :: FloatingType e) of
        TypeFloat {}   -> C2C
        TypeDouble {}  -> Z2Z
        TypeCFloat {}  -> C2C
        TypeCDouble {} -> Z2Z
    cudaFFT' p arr = deinterleave sh (foreignAcc ff pure1 (interleave arr))
      where
        ff = CUDAForeignAcc "foreignFFT" foreignFFT
        -- Unfortunately the pure version of the function needs to be wrapped in
        -- interleave and deinterleave to match how the foreign version works.
        --
        -- RCE: Do the interleaving and deinterleaving in foreignFFT
        --
        -- TLM: The interleaving might get fused into other parts of the
        --      computation and thus be okay. We should really support multi types
        --      such as float2 instead.
        --
        pure1 = interleave . p . deinterleave sh
        sign = signOfMode mode :: Int
        foreignFFT :: CUDA.Stream -> Array DIM1 e -> CIO (Array DIM1 e)
        foreignFFT stream arr' = do
          output <- allocateArray (S.shape arr')
          iptr <- floatingDevicePtr arr'
          optr <- floatingDevicePtr output
          --Execute
          liftIO $
            do setStream hndl stream
               execute iptr optr
          return output
        execute :: CUDA.DevicePtr e -> CUDA.DevicePtr e -> IO ()
        execute iptr optr =
          case (floatingType :: FloatingType e) of
            TypeFloat {} -> execC2C hndl iptr optr sign
            TypeDouble {} -> execZ2Z hndl iptr optr sign
            TypeCFloat {} ->
              execC2C hndl (CUDA.castDevPtr iptr) (CUDA.castDevPtr optr) sign
            TypeCDouble {} ->
              execZ2Z hndl (CUDA.castDevPtr iptr) (CUDA.castDevPtr optr) sign
        floatingDevicePtr :: Vector e -> CIO (CUDA.DevicePtr e)
        floatingDevicePtr v =
          case (floatingType :: FloatingType e) of
            TypeFloat {}   -> singleDevicePtr v
            TypeDouble {}  -> singleDevicePtr v
            TypeCFloat {}  -> CUDA.castDevPtr <$> singleDevicePtr v
            TypeCDouble {} -> CUDA.castDevPtr <$> singleDevicePtr v
        singleDevicePtr
          :: DevicePtrs (EltRepr e) ~ ((), CUDA.DevicePtr b)
          => Vector e -> CIO (CUDA.DevicePtr b)
        singleDevicePtr v = P.snd <$> devicePtrsOfArray v

{-# RULES
"interleave/deinterleave" forall sh x .
                          deinterleave sh (interleave x) = x
"deinterleave/interleave" forall sh x .
                          interleave (deinterleave sh x) = x
 #-}

{-# NOINLINE interleave #-}

interleave
  :: (Shape sh, Elt e)
  => Acc (Array sh (Complex e)) -> Acc (Vector e)
interleave arr = generate sh swizzle
  where
    sh = index1 (2 * A.size arr)
    swizzle ix =
      let i = indexHead ix
          v = arr A.!! (i `div` 2)
      in i `mod` 2 ==* 0 ? (real v, imag v)

{-# NOINLINE deinterleave #-}

deinterleave
  :: (Shape sh, Elt e)
  => sh -> Acc (Vector e) -> Acc (Array sh (Complex e))
deinterleave (constant -> sh) arr =
  generate
    sh
    (\ix ->
        let i = toIndex sh ix * 2
        in lift (arr A.!! i :+ arr A.!! (i + 1)))
