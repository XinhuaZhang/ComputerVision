{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.PairwiseEntropy.Histogram
  ( HistBinType
  , KdHistParams(..)
  , Bin(..)
  , KdHist(..)
  , build
  , computeMarginalHistogram
  , intersection
  , computeSVMKernel
  , entropy
  , getNumDims
  ) where

import           Control.Arrow
import           Control.DeepSeq
import           CV.Utility.Parallel
import           Data.Binary
import           Data.Int
import           Data.List           as L
import           Data.Vector.Unboxed as VU
import           GHC.Generics
import           Prelude             as P

type HistBinType = Word

data KdHistParams = KdHistParams
  { numBins         :: !Int
  , binWidth        :: !Double
  , negativeBinFlag :: !Bool -- true: 2*numBins - 1 bins; false: numBins bins
  , vecLen          :: !Int -- the length of a (Verctor Int) that will be convert to an Int.
  } deriving (Show, Read, Generic)

instance NFData KdHistParams where
  rnf (KdHistParams nd bw nbf vl) = nd `seq` bw `seq` nbf `seq` vl `seq` ()

instance Binary KdHistParams where
  put (KdHistParams nd bw nbf vl) = do
    put nd
    put bw
    put nbf
    put vl
  get = do
    nd <- get
    bw <- get
    nbf <- get
    vl <- get
    return $! KdHistParams nd bw nbf vl

data Bin a =
  Bin (Vector HistBinType)
      !a
  deriving (Show, Read, Generic)

instance (NFData a) =>
         NFData (Bin a) where
  rnf (Bin vec x) = rnf vec `seq` x `seq` ()

instance (Binary a) =>
         Binary (Bin a) where
  put (Bin vec n) = do
    put . VU.toList $ vec
    put n
  get = do
    xs <- get
    n <- get
    return $! Bin (VU.fromList xs) n

instance Functor Bin where
  fmap f (Bin vec x) = Bin vec (f x)

data KdHist a = KdHist
  { params    :: KdHistParams
  , histogram :: [Bin a]
  } deriving (Show, Read)

instance (NFData a) =>
         NFData (KdHist a) where
  rnf (KdHist params' hist) = rnf params' `seq` rnf hist

instance (Binary a) =>
         Binary (KdHist a) where
  put (KdHist p h) = do
    put p
    put h
  get = do
    p <- get
    h <- get
    return $! KdHist p h

instance Functor KdHist where
  fmap f (KdHist p h) = KdHist p (L.map (fmap f) h)

-- Here assuming every value is greater than zero.
build :: KdHistParams -> [Vector Double] -> KdHist Int
build params' [] = KdHist params' []
build params'@(KdHistParams numBins' binWidth' negativeBinFlag' len) xs =
  KdHist params' . merge . L.zip ys . L.repeat $ 1
  where
    m =
      fromIntegral $
      if negativeBinFlag'
        then 2 * numBins' - 1
        else numBins'
    ys =
      P.map
        (VU.fromList .
         P.map
           (\vec ->
               VU.foldl' (\a (i, b) -> a + b * (m ^ i)) 0 $
               VU.zip
                 (VU.generate
                    (VU.length vec)
                    (\x -> fromIntegral x :: HistBinType))
                 vec) .
         splitVec . VU.map locate)
        xs
    locate :: Double -> HistBinType
    locate x
      | negativeBinFlag' && (y <= (-n)) = 0
      | negativeBinFlag' && y >= n = 2 * n
      | negativeBinFlag' && (y < n && y > (-n)) = y + n
      | y > n = n
      | otherwise = y
      where
        y = floor (x / binWidth')
        n = fromIntegral $ numBins' - 1
    splitVec
      :: (Unbox a)
      => Vector a -> [Vector a]
    splitVec xs'
      | VU.null xs' = []
      | otherwise = as : splitVec bs
      where
        (as, bs) = VU.splitAt len xs'

computeMarginalHistogram
  :: (Num a)
  => KdHist a -> [Int] -> KdHist a
computeMarginalHistogram hist@(KdHist p bins) index =
  KdHist p .
  merge .
  L.map
    (\(Bin vec x) ->
        ( VU.fromList . L.map (\i -> vec VU.! i) . ascentDiff fullIndies $ index
        , x)) $
  bins
  where
    fullIndies = [0 .. getNumDims hist - 1]

{-# INLINE ascentDiff #-}

ascentDiff
  :: (Ord a)
  => [a] -> [a] -> [a]
ascentDiff xs [] = xs
ascentDiff [] _ = []
ascentDiff (x:xs) (y:ys)
  | x == y = ascentDiff xs ys
  | x < y = x : ascentDiff xs (y : ys)
  | otherwise = ascentDiff (x : xs) ys

{-# INLINE merge #-}

merge
  :: (Num a)
  => [(Vector HistBinType, a)] -> [Bin a]
merge =
  L.map (uncurry Bin . (L.head *** L.sum) . L.unzip) .
  L.groupBy (\x y -> fst x == fst y) . L.sortOn fst

intersection
  :: (Ord a, Num a)
  => KdHist a -> KdHist a -> a
intersection (KdHist _ xs') (KdHist _ ys') = result
  where
    result = func xs' ys'
    func
      :: (Ord a, Num a)
      => [Bin a] -> [Bin a] -> a
    func [] _ = 0
    func _ [] = 0
    func (Bin x a:xs) (Bin y b:ys)
      | x == y = min a b + func xs ys
      | x < y = func xs (Bin y b : ys)
      | x > y = func (Bin x a : xs) ys

computeSVMKernel
  :: (Ord a, Num a, NFData a)
  => ParallelParams -> [KdHist a] -> [[a]]
computeSVMKernel parallelParams xs =
  parMapChunk parallelParams rdeepseq (\x -> P.map (intersection x) xs) xs

entropy
  :: (Num a, Floating a)
  => KdHist a -> a
entropy (KdHist _ bins) = -1 * (L.sum . L.map ((\x -> x * log x) . (/ s)) $ xs)
  where
    !xs = L.map (\(Bin _ n) -> n) bins
    !s = L.sum xs

{-# INLINE getNumDims #-}

getNumDims :: KdHist a -> Int
getNumDims (KdHist _ []) = error "getNumDims: This histogram is zero."
getNumDims (KdHist _ (Bin vec _:_)) = VU.length vec
