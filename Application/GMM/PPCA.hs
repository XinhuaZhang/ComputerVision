{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveGeneric #-}
module Application.GMM.PPCA where

import           Data.Binary
import           Data.Foldable as F
import           Data.Matrix   as Mat
import           Data.Time
import           Data.Vector   as V
import           GHC.Generics
import           Prelude       as P
import           System.Random

data PPCA =
  PPCA {numDims  :: Int           -- D
       ,numZDims :: Int           -- M
       ,wMat     :: Matrix Double -- D X M
       ,muMat    :: Matrix Double -- D X 1
       ,sigma    :: Double}
  deriving (Show,Generic)

data PPCAInitParams =
  PPCAInitParams {numPrincipal :: Int
                 ,wRange       :: (Double,Double)
                 ,muRange      :: (Double,Double)
                 ,sigmaRange   :: (Double,Double)}
  deriving ((Show))

instance Binary PPCA where
  put (PPCA nd nzd w mu sigma') =
    do put nd
       put nzd
       put $ Mat.toLists w
       put $ Mat.toLists mu
       put sigma'
  get =
    do nd <- get
       nzd <- get
       ws <- get
       mus <- get
       sigma' <- get
       return (PPCA nd
                    nzd
                    (Mat.fromLists ws)
                    (Mat.fromLists mus)
                    sigma')

-- initialization
randomRList :: (RandomGen g,Random a)
            => Int -> (a,a) -> g -> ([a],g)
randomRList len bound gen
  | len > 0 =
    (\(xs,g) -> (x : xs,g)) $
    randomRList (len - 1)
                bound
                newGen
  | otherwise = ([],gen)
  where (x,newGen) = randomR bound gen

randomPPCA
  :: (RandomGen g)
  => PPCAInitParams -> Int -> g -> (PPCA,g)
randomPPCA (PPCAInitParams nM wR muR sigmaR) nD gen =
  (PPCA nD
        nM
        (Mat.fromList nD nM w)
        (Mat.fromList nD 1 mu)
        s
  ,gen3)
  where (w,gen1) =
          randomRList (nD * nM)
                      wR
                      gen
        (mu,gen2) = randomRList nD muR gen1
        (s,gen3) = randomR sigmaR gen2


-- The normalization term (2 * pi) ** (-(P.fromIntegral nd) / 2) is not needed.
ppcaP
  :: PPCA -> Matrix Double -> Double
ppcaP model@(PPCA nd _nzd w' mu' sigma') x =
  (exp (-1 * y3 / (2 * sigma' ^ (2 :: Int)))) / (sigma' ^ nd)
  where wz = w' `multStd2` z
        y1 = elementwiseUnsafe (-) x wz
        y2 = elementwiseUnsafe (-) y1 mu'
        y3 = F.foldl' (\a b -> a + b ^ (2 :: Int)) 0 $ y2
        z = computeLatentZ model x


ppcaPVec :: PPCA -> V.Vector (Matrix Double) -> V.Vector Double
ppcaPVec model@(PPCA _nd _nzd w' mu' sigma') vecX =
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
computeLatentZ (PPCA _nD nM w' mu' sigma') x =
  invM `multStd2` wt `multStd2` (elementwiseUnsafe (-) x mu')
  where wt = transpose w'
        diagSigma = diagonal 0 (V.replicate nM sigma')
        m =
          elementwiseUnsafe (+)
                            diagSigma
                            (wt `multStd2` w')
        invM =
          case inverse m of
            Left msg -> error msg
            Right y  -> y

computeLatentZVec :: PPCA -> V.Vector (Matrix Double) -> V.Vector (Matrix Double)
computeLatentZVec (PPCA _nD nM w' mu' sigma') =
  V.map (\x -> mwt `multStd2` (elementwiseUnsafe (-) x mu'))
  where wt = transpose w'
        diagSigma = diagonal 0 (V.replicate nM sigma')
        m =
          elementwiseUnsafe (+)
                            diagSigma
                            (wt `multStd2` w')
        invM =
          case inverse m of
            Left msg -> error msg
            Right y  -> y
        mwt = invM `multStd2` wt
