module Application.HandWriting.Types where

import           Data.Binary
import           Data.List           as L
import           Data.Vector.Unboxed as VU

data OfflineCharacter = OfflineCharacter
  { tag       :: !Word16
  , width     :: !Word16
  , height    :: !Word16
  , character :: Vector Word8
  }

instance Show OfflineCharacter where
  show (OfflineCharacter t w h _) =
    "{OfflineCharacter " L.++ show t L.++ " " L.++ show w L.++ " " L.++ show h L.++
    "}"


data SparseOfflineCharacter = SparseOfflineCharacter
  { sparseOCTag       :: !Word16
  , sparseOCWidth     :: !Word16
  , sparseOCHeight    :: !Word16
  , sparseOCCharacter :: Vector (Word16,Word8)
  }

instance Binary SparseOfflineCharacter where
  put (SparseOfflineCharacter t w h c) = do
    put t
    put w
    put h
    put . toList $ c
  get = do
    t <- get
    w <- get
    h <- get
    c <- get
    return $! SparseOfflineCharacter t w h . fromList $ c
