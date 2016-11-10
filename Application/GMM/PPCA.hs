{-# LANGUAGE DeriveGeneric #-}

module Application.GMM.PPCA where

import           Data.Binary
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
       ,sigma    :: Double        -- sigma here is actually a sigma^2
       }
  deriving (Show,Generic)

data PPCAInitParams =
  PPCAInitParams {numPrincipal :: Int
                 ,wRange       :: (Double,Double)
                 ,muRange      :: (Double,Double)
                 ,sigmaRange   :: (Double,Double)}
  deriving (Show)

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
randomRList :: (RandomGen g
               ,Random a)
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
ppcaP :: PPCA -> Matrix Double -> Double
ppcaP model@(PPCA nd _nzd w' mu' sigma') x =
  ((detLU c) ** (-0.5)) *
  (exp ((-0.5) * (((transpose xmu) `multStd2` invC `multStd2` xmu)) Mat.! (1,1)))
  where xmu = elementwiseUnsafe (-) x mu'
        invM = computeInvM model
        invC = computeInvC model invM
        c =
          elementwiseUnsafe (+)
                            (diagonal 0 (V.replicate nd sigma'))
                            (w' `multStd2` (transpose w'))

ppcaP'
  :: PPCA -> Matrix Double -> Matrix Double -> Double
ppcaP' model@(PPCA nd _nzd w' mu' sigma') invM x =
  ((detLU c) ** (-0.5)) *
  (exp ((-0.5) * (((transpose xmu) `multStd2` invC `multStd2` xmu)) Mat.! (1,1)))
  where xmu = elementwiseUnsafe (-) x mu'
        invC = computeInvC model invM
        c =
          elementwiseUnsafe (+)
                            (diagonal 0 (V.replicate nd sigma'))
                            (w' `multStd2` (transpose w'))

ppcaPVec
  :: PPCA -> V.Vector (Matrix Double) -> V.Vector Double
ppcaPVec model@(PPCA nd _nzd w' mu' sigma') =
  V.map (\x ->
           ((detLU c) ** (-0.5)) *
           (exp ((-0.5) *
                 (((transpose (xmu x)) `multStd2` invC `multStd2` (xmu x))) Mat.!
                 (1,1))))
  where xmu x = elementwiseUnsafe (-) x mu'
        invM = computeInvM model
        invC = computeInvC model invM
        c =
          elementwiseUnsafe (+)
                            (diagonal 0 (V.replicate nd sigma'))
                            (w' `multStd2` (transpose w'))

ppcaPVec' :: PPCA
          -> Matrix Double
          -> V.Vector (Matrix Double)
          -> V.Vector Double
ppcaPVec' model@(PPCA nd _nzd w' mu' sigma') invM =
  V.map (\x ->
           ((detLU c) ** (-0.5)) *
           (exp ((-0.5) *
                 (((transpose (xmu x)) `multStd2` invC `multStd2` (xmu x))) Mat.!
                 (1,1))))
  where xmu x = elementwiseUnsafe (-) x mu'
        invC = computeInvC model invM
        c =
          elementwiseUnsafe (+)
                            (diagonal 0 (V.replicate nd sigma'))
                            (w' `multStd2` (transpose w'))

computeLatentZ
  :: PPCA -> Matrix Double -> Matrix Double
computeLatentZ model@(PPCA _nD nM w' mu' sigma') x =
  invM `multStd2` wt `multStd2` (elementwiseUnsafe (-) x mu')
  where wt = transpose w'
        invM = computeInvM model

computeLatentZ'
  :: PPCA -> Matrix Double -> Matrix Double -> Matrix Double
computeLatentZ' model@(PPCA _nD nM w' mu' sigma') invM x =
  invM `multStd2` wt `multStd2` (elementwiseUnsafe (-) x mu')
  where wt = transpose w'

computeLatentZVec :: PPCA
                  -> V.Vector (Matrix Double)
                  -> V.Vector (Matrix Double)
computeLatentZVec model@(PPCA _nD nM w' mu' sigma') =
  V.map (\x -> mwt `multStd2` (elementwiseUnsafe (-) x mu'))
  where invM = computeInvM model
        wt = transpose w'
        mwt = invM `multStd2` wt

computeLatentZVec' :: PPCA
                   -> Matrix Double
                   -> V.Vector (Matrix Double)
                   -> V.Vector (Matrix Double)
computeLatentZVec' (PPCA _nD nM w' mu' sigma') invM =
  V.map (\x -> mwt `multStd2` (elementwiseUnsafe (-) x mu'))
  where wt = transpose w'
        mwt = invM `multStd2` wt

computeInvM :: PPCA -> Matrix Double
computeInvM (PPCA _nD nM w' mu' sigma') = invM
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

computeInvC
  :: PPCA -> Matrix Double -> Matrix Double
computeInvC (PPCA nD _nM w' mu' sigma') invM =
  scaleMatrix
    (1 / (sigma' ^ 2))
    (elementwiseUnsafe (-)
                       (identity nD)
                       (w' `multStd2` invM `multStd2` wt))
  where wt = transpose w'
