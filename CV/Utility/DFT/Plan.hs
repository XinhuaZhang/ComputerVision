{-# LANGUAGE DeriveGeneric #-}
module CV.Utility.DFT.Plan
  ( module HM
  , DFTPlanID(..)
  , DFTType(..)
  , DFTPlan
  , FFTWLock
  , getFFTWLock
  , getDFTPlan
  , getEmptyPlan
  , dftExecuteWithPlan
  , dftExecute
  , dft2dPlan
  , idft2dPlan
  , dft1dGPlan
  , idft1dGPlan
  ) where

import           Control.Concurrent.MVar
import           CV.Utility.DFT.FFI
import           CV.Utility.FFT.Base          (Flag (..), Sign (..), measure,
                                               unFlag, unSign)
import           Data.Complex
import           Data.Hashable
import           Data.HashMap.Strict          as HM
import           Data.List                    as L
import           Data.Vector.Storable         as VS
import           Data.Vector.Storable.Mutable as VSM
import           Foreign.Marshal.Array
import           GHC.Generics                 (Generic)

data DFTType
  = DFT1DG
  | IDFT1DG
  | DFT2D
  | IDFT2D
  deriving (Show, Eq, Generic)

instance Hashable DFTType

data DFTPlanID = DFTPlanID
  { dftType :: DFTType
  , dftDims :: [Int]
  , dftIdx  :: [Int]
  } deriving (Show, Eq, Generic)

instance Hashable DFTPlanID

type DFTPlan = HashMap DFTPlanID Plan

type FFTWLock = MVar ()

{-# INLINE getFFTWLock #-}

getFFTWLock :: IO FFTWLock
getFFTWLock = newMVar ()

{-# INLINE getEmptyPlan #-}

getEmptyPlan :: DFTPlan
getEmptyPlan = HM.empty

{-# INLINE getDFTPlan #-}

getDFTPlan :: DFTPlan -> DFTPlanID -> Plan
getDFTPlan plan planID =
  case HM.lookup planID plan of
    Nothing -> error $ "getDFTPlan: couldn't find plan for ID " L.++ show planID
    Just p -> p
    
{-# INLINE dftExecuteWithPlan #-}

dftExecuteWithPlan :: Plan
                   -> Int
                   -> VS.Vector (Complex Double)
                   -> IO (VS.Vector (Complex Double))
dftExecuteWithPlan plan size vec = do
  v <- VSM.new size
  VS.unsafeWith vec $ \ip -> VSM.unsafeWith v $ \op -> c_execute_dft plan ip op
  VS.freeze v

{-# INLINE dftExecute #-}

dftExecute
  :: DFTPlan
  -> DFTPlanID
  -> VS.Vector (Complex Double)
  -> IO (VS.Vector (Complex Double))
dftExecute hashMap planID@(DFTPlanID t d i) vec =
  case HM.lookup planID hashMap of
    Nothing -> error $ "dftExecute: couldn't find plan for ID " L.++ show planID
    Just plan -> do
      u <- dftExecuteWithPlan plan (L.product d) vec
      if dftType planID == IDFT1DG || dftType planID == IDFT2D
        then let size =
                   L.product .
                   L.take (L.last i - L.head i + 1) . L.drop (L.head i) $
                   d
             in return $
                if L.null i
                  then VS.map (/ (fromIntegral . L.product $ d)) u
                  else VS.map (/ (fromIntegral size)) u
        else return u

{-# INLINE dft2dG #-}

dft2dG
  :: MVar ()
  -> Int
  -> Int
  -> VS.Vector (Complex Double)
  -> Sign
  -> Flag
  -> IO (Plan, VS.Vector (Complex Double))
dft2dG lock' rows cols vec sign flag = do
  v <- VSM.new (rows * cols)
  x <- takeMVar lock'
  p <-
    VS.unsafeWith vec $ \ip ->
      VSM.unsafeWith v $ \op -> do
        c_plan_dft_2d
          (fromIntegral rows)
          (fromIntegral cols)
          ip
          op
          (unSign sign)
          (unFlag flag)
  putMVar lock' x
  c_execute p
  u <- VS.freeze v
  return (p, u)

{-# INLINE dft2dPlan #-}

dft2dPlan
  :: FFTWLock
  -> DFTPlan
  -> Int
  -> Int
  -> VS.Vector (Complex Double)
  -> IO (DFTPlan, VS.Vector (Complex Double))
dft2dPlan lock' hashMap rows cols vec =
  case HM.lookup planID hashMap of
    Nothing -> do
      (p, v) <- dft2dG lock' rows cols vec DFTForward measure
      return (HM.insert planID p hashMap, v)
    Just p -> do
      v <- dftExecuteWithPlan p (rows * cols) vec
      return (hashMap, v)
  where
    planID = DFTPlanID DFT2D [rows, cols] []

{-# INLINE idft2dPlan #-}

idft2dPlan
  :: FFTWLock
  -> DFTPlan
  -> Int
  -> Int
  -> VS.Vector (Complex Double)
  -> IO (DFTPlan, VS.Vector (Complex Double))
idft2dPlan lock' hashMap rows cols vec =
  case HM.lookup planID hashMap of
    Nothing -> do
      (p, v) <- dft2dG lock' rows cols vec DFTBackward measure
      return
        (HM.insert planID p hashMap, VS.map (/ (fromIntegral $ rows * cols)) v)
    Just p -> do
      v <- dftExecuteWithPlan p (rows * cols) vec
      return (hashMap, VS.map (/ (fromIntegral $ rows * cols)) v)
  where
    planID = DFTPlanID IDFT2D [rows, cols] []

-- This is a generalied 1d dft, the 1 dimension can be many dimensions
-- which are ascending ordered and continued. For example, given a N
-- dimension array, the generalized 1d dft dimension is
-- either [0,1..M] or [M,M+1.. N-1], where 0 <= M and M <= N-1
-- the dimension corresponding to the largest index spins the fastest.

{-# INLINE dft1dGGeneric #-}

dft1dGGeneric
  :: MVar ()
  -> [Int]
  -> [Int]
  -> VS.Vector (Complex Double)
  -> Sign
  -> Flag
  -> IO (Plan, VS.Vector (Complex Double))
dft1dGGeneric lock' dims dftIndex vec sign flag
  | L.and (L.zipWith (\a b -> a + 1 == b) dftIndex . L.tail $ dftIndex) &&
      (not . L.null $ dftIndex) &&
      (L.head dftIndex == 0 || L.last dftIndex == rank - 1) = do
    v <- VSM.new . L.product $ dims
    x <- takeMVar lock'
    p <-
      VS.unsafeWith vec $ \ip ->
        VSM.unsafeWith v $ \op ->
          withArray (L.map fromIntegral dftDims) $ \n -> do
            let totalNum = L.product dims
                dftNum = L.product dftDims
                stride =
                  if L.last dftIndex == rank - 1
                    then 1
                    else L.product . L.drop (1 + L.last dftIndex) $ dims
                dist =
                  if L.last dftIndex == rank - 1
                    then dftNum
                    else 1
            c_plan_many_dft
              (fromIntegral dftRank)
              n
              (fromIntegral $ div totalNum dftNum)
              ip
              n
              (fromIntegral stride)
              (fromIntegral dist)
              op
              n
              (fromIntegral stride)
              (fromIntegral dist)
              (unSign sign)
              (unFlag flag)
    putMVar lock' x
    c_execute p
    u <- VS.freeze v
    return (p, u)
  | otherwise =
    error
      "dft1dG: dimension list doesn't satisify the restriction of generalized 1d dft."
  where
    rank = L.length dims
    dftRank = L.length dftIndex
    dftDims =
      L.take (L.last dftIndex - L.head dftIndex + 1) . L.drop (L.head dftIndex) $
      dims

{-# INLINE dft1dGPlan #-}

dft1dGPlan
  :: FFTWLock
  -> DFTPlan
  -> [Int]
  -> [Int]
  -> VS.Vector (Complex Double)
  -> IO (DFTPlan, VS.Vector (Complex Double))
dft1dGPlan lock' hashMap dims dftIndex vec =
  case HM.lookup planID hashMap of
    Nothing -> do
      (p, v) <- dft1dGGeneric lock' dims dftIndex vec DFTForward measure
      return (HM.insert planID p hashMap, v)
    Just p -> do
      v <- dftExecuteWithPlan p (L.product dims) vec
      return (hashMap, v)
  where
    planID = DFTPlanID DFT1DG dims dftIndex

{-# INLINE idft1dGPlan #-}

idft1dGPlan
  :: FFTWLock
  -> DFTPlan
  -> [Int]
  -> [Int]
  -> VS.Vector (Complex Double)
  -> IO (DFTPlan, VS.Vector (Complex Double))
idft1dGPlan lock' hashMap dims dftIndex vec =
  case HM.lookup planID hashMap of
    Nothing -> do
      (p, v) <- dft1dGGeneric lock' dims dftIndex vec DFTBackward measure
      return (HM.insert planID p hashMap, VS.map (/ size) v)
    Just p -> do
      v <- dftExecuteWithPlan p (L.product dims) vec
      return (hashMap, VS.map (/ size) v)
  where
    planID = DFTPlanID IDFT1DG dims dftIndex
    size =
      fromIntegral .
      L.product .
      L.take (L.last dftIndex - L.head dftIndex + 1) . L.drop (L.head dftIndex) $
      dims
