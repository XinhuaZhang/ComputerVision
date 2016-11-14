{-# LANGUAGE DeriveGeneric #-}

module Application.GMM.PPCA where

import           Application.GMM.Matrix
import           Data.Binary
import           Data.Matrix            as Mat
import           Data.Time
import           Data.Vector            as V
import           Data.Vector.Unboxed    as VU
import           GHC.Generics
import           Prelude                as P
import           System.Random

data PPCA = PPCA
  { numDims  :: Int              -- D
  , numZDims :: Int              -- M
  , wMat     :: Matrix Double    -- D X M
  , muMat    :: VU.Vector Double -- D X 1
  , sigma    :: Double           -- sigma here is actually a sigma^2
  } deriving (Show, Generic)

data PPCAInitParams = PPCAInitParams
  { numPrincipal :: Int
  , wRange       :: (Double, Double)
  , muRange      :: (Double, Double)
  , sigmaRange   :: (Double, Double)
  } deriving (Show)

instance Binary PPCA where
  put (PPCA nd nzd w mu sigma') = do
    put nd
    put nzd
    put $ Mat.toLists w
    put $ VU.toList mu
    put sigma'
  get = do
    nd <- get
    nzd <- get
    ws <- get
    mus <- get
    sigma' <- get
    return (PPCA nd nzd (Mat.fromLists ws) (VU.fromListN nd mus) sigma')

-- initialization
randomRList
  :: (RandomGen g, Random a)
  => Int -> (a, a) -> g -> ([a], g)
randomRList len bound gen
  | len > 0 = (\(xs, g) -> (x : xs, g)) $ randomRList (len - 1) bound newGen
  | otherwise = ([], gen)
  where
    (x, newGen) = randomR bound gen

randomPPCA
  :: (RandomGen g)
  => PPCAInitParams -> Int -> g -> (PPCA, g)
randomPPCA (PPCAInitParams nM wR muR sigmaR) nD gen =
  (PPCA nD nM (Mat.fromList nD nM w) (VU.fromListN nD mu) s, gen3)
  where
    (w, gen1) = randomRList (nD * nM) wR gen
    (mu, gen2) = randomRList nD muR gen1
    (s, gen3) = randomR sigmaR gen2

-- The normalization term (2 * pi) ** (-(P.fromIntegral nd) / 2) is not needed.
ppcaP :: PPCA -> VU.Vector Double -> Double
ppcaP model@(PPCA nd _nzd w' mu' sigma') x =
  ((detLU c) ** (-0.5)) * (exp ((-0.5) * (xtwx xmu invCVec)))
  where
    xmu = VU.zipWith (-) x mu'
    invM = computeInvM model
    invC = computeInvC model invM
    invCVec = V.fromList . P.map VU.fromList . Mat.toLists $ invC
    c =
      elementwiseUnsafe
        (+)
        (diagonal 0 (V.replicate nd sigma'))
        (w' * (transpose w'))

ppcaP' :: PPCA -> Matrix Double -> VU.Vector Double -> Double
ppcaP' model@(PPCA nd _nzd w' mu' sigma') invM x =
  ((detLU c) ** (-0.5)) * (exp ((-0.5) * (xtwx xmu invCVec)))
  where
    xmu = VU.zipWith (-) x mu'
    xmuMat = colVector . VU.convert $ xmu
    invC = computeInvC model invM
    invCVec = V.fromList . P.map VU.fromList . Mat.toLists $ invC
    c =
      elementwiseUnsafe
        (+)
        (diagonal 0 (V.replicate nd sigma'))
        (w' * (transpose w'))

ppcaPVec :: PPCA -> V.Vector (VU.Vector Double) -> V.Vector Double
ppcaPVec model@(PPCA nd _nzd w' mu' sigma') =
  V.map (\x -> ((detLU c) ** (-0.5)) * (exp ((-0.5) * (xtwx (xmu x) invCVec))))
  where
    xmu x = VU.zipWith (-) x mu'
    invM = computeInvM model
    invC = computeInvC model invM
    invCVec = V.fromList . P.map VU.fromList . Mat.toLists $ invC
    c =
      elementwiseUnsafe
        (+)
        (diagonal 0 (V.replicate nd sigma'))
        (w' * (transpose w'))

ppcaPVec' :: PPCA -> Matrix Double -> V.Vector (VU.Vector Double) -> V.Vector Double
ppcaPVec' model@(PPCA nd _nzd w' mu' sigma') invM =
  V.map (\x -> ((detLU c) ** (-0.5)) * (exp ((-0.5) * (xtwx (xmu x) invCVec))))
  where
    xmu x = VU.zipWith (-) x mu'
    invC = computeInvC model invM
    invCVec = V.fromList . P.map VU.fromList . Mat.toLists $ invC
    c =
      elementwiseUnsafe
        (+)
        (diagonal 0 (V.replicate nd sigma'))
        (w' * (transpose w'))

computeLatentZ :: PPCA -> VU.Vector Double -> Matrix Double
computeLatentZ model@(PPCA _nD nM w' mu' sigma') x =
  invM * wt * (colVector . VU.convert $ VU.zipWith (-) x mu')
  where
    wt = transpose w'
    invM = computeInvM model

computeLatentZ' :: PPCA -> Matrix Double -> VU.Vector Double -> Matrix Double
computeLatentZ' model@(PPCA _nD nM w' mu' sigma') invM x =
  invM * wt * (colVector . VU.convert $ VU.zipWith (-) x mu')
  where
    wt = transpose w'

computeLatentZVec :: PPCA
                  -> V.Vector (VU.Vector Double)
                  -> V.Vector (Matrix Double)
computeLatentZVec model@(PPCA _nD nM w' mu' sigma') =
  V.map (\x -> mwt * (colVector . VU.convert $ VU.zipWith (-) x mu'))
  where
    invM = computeInvM model
    wt = transpose w'
    mwt = invM * wt

computeLatentZVec' :: PPCA
                   -> Matrix Double
                   -> V.Vector (VU.Vector Double)
                   -> V.Vector (Matrix Double)
computeLatentZVec' (PPCA _nD nM w' mu' sigma') invM =
  V.map (\x -> mwt * (colVector . VU.convert $ VU.zipWith (-) x mu'))
  where
    wt = transpose w'
    mwt = invM * wt

computeInvM :: PPCA -> Matrix Double
computeInvM (PPCA _nD nM w' mu' sigma') = invM
  where
    wt = transpose w'
    diagSigma = diagonal 0 (V.replicate nM sigma')
    m = elementwiseUnsafe (+) diagSigma (wt * w')
    invM =
      case inverse m of
        Left msg -> error msg
        Right y  -> y

computeInvC :: PPCA -> Matrix Double -> Matrix Double
computeInvC (PPCA nD _nM w' mu' sigma') invM =
  scaleMatrix
    (1 / (sigma' ^ 2))
    (elementwiseUnsafe (-) (identity nD) (w' * invM * wt))
  where
    wt = transpose w'
