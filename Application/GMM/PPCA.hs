{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
module Application.GMM.PPCA where

import           Data.Binary
import           Data.Foldable as F
import           Data.Matrix   as Mat
import           Data.Vector   as V
import           GHC.Generics
import           Prelude       as P

data PPCA =
  PPCA {numDims :: Int
       ,wMat    :: Matrix Double
       ,muMat   :: Matrix Double
       ,sigma   :: Double}
  deriving (Show,Generic)

instance Binary PPCA where
  put (PPCA nd w mu sigma') =
    do put nd
       put $ Mat.toLists w
       put $ Mat.toLists mu
       put sigma'
  get =
    do nd <- get
       ws <- get
       mus <- get
       sigma' <- get
       return (PPCA nd
                    (Mat.fromLists ws)
                    (Mat.fromLists mus)
                    sigma')

-- The normalization term (2 * pi * sigma' ^ (2 :: Int)) ** (-(P.fromIntegral nd) / 2) is not needed.
ppcaP
  :: PPCA -> Matrix Double -> Double
ppcaP model@(PPCA nd w' mu' sigma') x =
  (exp (-1 * y3 / (2 * sigma' ^ (2 :: Int))))
  where wz = w' `multStd2` z
        y1 = elementwiseUnsafe (-) x wz
        y2 = elementwiseUnsafe (-) y1 mu'
        y3 = F.foldl' (\a b -> a + b ^ (2 :: Int)) 0 $ y2
        z = computeLatentZ model x
        

ppcaPVec :: PPCA -> V.Vector (Matrix Double) -> V.Vector Double
ppcaPVec model@(PPCA _nd w' mu' sigma') vecX =
  V.zipWith (\x z ->
               exp . (\y -> -1 * y / (2 * sigma' ^ (2 :: Int))) . squareSum $
               elementwiseUnsafe
                 (-)
                 (elementwiseUnsafe (-)
                                    x
                                    (w' `multStd2` z))
                 mu')
            vecX
            vecZ
  where squareSum x = F.foldl' (\a b -> a + b ^ (2 :: Int)) 0 $ x
        !vecZ = computeLatentZVec model vecX


computeLatentZ :: PPCA -> Matrix Double -> Matrix Double
computeLatentZ (PPCA nd w' mu' sigma') x =
  invM `multStd2` wt `multStd2` (elementwiseUnsafe (-) x mu')
  where wt = transpose w'
        diagSigma = diagonal 0 (V.replicate nd sigma')
        m =
          elementwiseUnsafe (+)
                            diagSigma
                            (wt `multStd2` w')
        invM =
          case inverse m of
            Left msg -> error msg
            Right y -> y
            
computeLatentZVec :: PPCA -> V.Vector (Matrix Double) -> V.Vector (Matrix Double)
computeLatentZVec (PPCA nd w' mu' sigma') =
  V.map (\x -> mwt `multStd2` (elementwiseUnsafe (-) x mu'))
  where wt = transpose w'
        diagSigma = diagonal 0 (V.replicate nd sigma')
        m =
          elementwiseUnsafe (+)
                            diagSigma
                            (wt `multStd2` w')
        invM =
          case inverse m of
            Left msg -> error msg
            Right y -> y
        mwt = invM `multStd2` wt
