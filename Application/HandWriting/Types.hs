module Application.HandWriting.Types where

import           Data.Int
import           Data.List           as L
import           Data.Vector.Unboxed as VU
import           Data.Word

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
