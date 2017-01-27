{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveGeneric #-}
module Application.PairwiseEntropy.Histogram where

import           Control.DeepSeq
import           CV.Utility.Parallel
import           Data.Binary
import           Data.Int
import           Data.List           as L
import qualified Data.Vector         as V
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

data Bin =
  Bin (Vector HistBinType)
      !Int
  deriving (Show, Read, Generic)

instance NFData Bin where
  rnf (Bin vec count) = rnf vec `seq` count `seq` ()

instance Binary Bin where
  put (Bin vec n) = do
    put . VU.toList $ vec
    put n
  get = do
    xs <- get
    n <- get
    return $! Bin (VU.fromList xs) n

data KdHist = KdHist
  { params    :: KdHistParams
  , histogram :: [Bin]
  } deriving (Show, Read)

instance NFData KdHist where
  rnf (KdHist params' hist) = rnf params' `seq` rnf hist

instance Binary KdHist where
  put (KdHist p h) = do
    put p
    put h
  get = do
    p <- get
    h <- get
    return $! KdHist p h

-- Here assuming every value is greater than zero.
build :: KdHistParams -> [Vector Double] -> KdHist
build params' [] = KdHist params' []
build params'@(KdHistParams numBins' binWidth' negativeBinFlag' len) xs =
  KdHist params' $ merge 1 zs
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
    zs = L.sort ys
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
    merge :: Int -> [Vector HistBinType] -> [Bin]
    merge _ [] = []
    merge _ [x] = [Bin x 1]
    merge !n [x, y]
      | x == y = [Bin x (n + 1)]
      | otherwise = [Bin x n, Bin y 1]
    merge !n (x:y:xs')
      | x == y = merge (n + 1) (y : xs')
      | otherwise = Bin x n : merge 1 (y : xs')

intersection :: KdHist -> KdHist -> Int
intersection (KdHist _ xs') (KdHist _ ys') = result
  where
    result = func xs' ys'
    func :: [Bin] -> [Bin] -> Int
    func [] _ = 0
    func _ [] = 0
    func (Bin x a:xs) (Bin y b:ys)
      | x == y = min a b + func xs ys
      | x < y = func xs (Bin y b : ys)
      | x > y = func (Bin x a : xs) ys

computeSVMKernel :: ParallelParams -> [KdHist] -> [[Int]]
computeSVMKernel parallelParams xs =
  parMapChunk parallelParams rdeepseq (\x -> P.map (intersection x) xs) xs

computeSVMKernelDouble :: ParallelParams -> [KdHist] -> [[Double]]
computeSVMKernelDouble parallelParams xs =
  sp $
  parMapChunkVector
    parallelParams
    rdeepseq
    (\(x, y) -> fromIntegral $ intersection x y) $!
  V.fromList
    [ (a, b)
    | a <- xs
    , b <- xs ]
  where
    sp zs
      | V.null zs = []
      | otherwise = V.toList as : sp bs
      where
        (as, bs) = V.splitAt (P.length xs) zs

entropy :: KdHist -> Double
entropy (KdHist _ bins) =
  -1 * (L.sum . L.map ((\x -> x * log x) . (\x -> fromIntegral x / s)) $ xs)
  where
    !xs = L.map (\(Bin _ n) -> n) bins
    !s = fromIntegral $ L.sum xs
