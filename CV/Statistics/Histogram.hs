{-# LANGUAGE BangPatterns #-}

module CV.Statistics.Histogram where

import           Control.Arrow
import           Control.DeepSeq
import           Data.Binary
import           Data.Int
import           Data.List           as L
import           Data.Vector.Unboxed as VU
import           Prelude             as P

type HistBinType = Word

data KdHistParams = KdHistParams
  { numBins         :: !Int
  , binWidth        :: !Double
  , negativeBinFlag :: !Bool -- true: 2*numBins - 1 bins; false: numBins bins
  , vecLen          :: !Int -- the length of a (Verctor Int) that will be convert to an Int.
  } deriving (Show, Read)

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
  deriving (Show, Read)

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
      if len == 1
        then P.map (VU.map locate) xs
        else P.map
               (VU.fromList .
                P.map
                  (\vec ->
                      VU.foldl' (\a (i, b) -> a + b * (m ^ i)) 0 $
                      VU.zip
                        (VU.generate
                           (VU.length vec)
                           (\x -> fromIntegral x :: HistBinType))
                        vec) .
                splitVec len . VU.map locate)
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
      => Int -> Vector a -> [Vector a]
    splitVec l xs'
      | VU.null xs' = []
      | otherwise = as : splitVec l bs
      where
        (as, bs) = VU.splitAt l xs'

{-# INLINE getNumDims #-}

getNumDims :: KdHist a -> Int
getNumDims (KdHist _ []) = error "getNumDims: This histogram is zero."
getNumDims (KdHist _ (Bin vec _:_)) = VU.length vec

intersection
  :: (Ord a, Num a)
  => KdHist a -> KdHist a -> a
intersection (KdHist _ xs') (KdHist _ ys') = go xs' ys'
  where
    go [] _ = 0
    go _ [] = 0
    go (Bin x a:xs) (Bin y b:ys)
      | x == y =
        let !m = min a b
            !n = go xs ys
        in m + n
      | x < y = go xs (Bin y b : ys)
      | x > y = go (Bin x a : xs) ys

{-# INLINE merge #-}

merge
  :: (Num a)
  => [(Vector HistBinType, a)] -> [Bin a]
merge =
  L.map (uncurry Bin . (L.head *** L.sum) . L.unzip) .
  L.groupBy (\x y -> fst x == fst y) . L.sortOn fst


-- Suppose the histogram is P(x0,x1..xn), given indexes [i1,i2..im]
-- where 0 <= i <= n, the result marginal distribution is
-- P(xi1,xi2..xim)
{-# INLINE computeMarginalHistogram #-}

computeMarginalHistogram
  :: (Num a)
  => KdHist a -> [Int] -> KdHist a
computeMarginalHistogram (KdHist p bins) index =
  case vecLen p of
    1 ->
      KdHist p .
      merge .
      L.map
        (\(Bin vec x) -> (VU.fromList . L.map (\i -> vec VU.! i) $ index, x)) $
      bins
    _ ->
      error
        "computeMarginalHistogram: this histogram is compressed and hence cannot be used to compute mariginal histogram."
