{-# LANGUAGE BangPatterns #-}
module Main where

import           Data.Matrix as Mat
import           Data.Vector as V
import Data.Vector.Unboxed as VU
import Data.List as L
import Application.GMM.Matrix 



main =
  do let n = 1000
         m = 1000000
         list = [1,2,3,4,5,6,7,8,9,10] :: [Double]
         vecU = VU.fromList list
         vecVecU = V.replicate m vecU
         listVecU = L.replicate m vecU
         vec = V.fromList  list  
         vecs = V.replicate m vec
         vecsU = V.map (V.convert) vecs :: V.Vector (VU.Vector Double)
         mat = colVector $ VU.convert vec
         mats = V.replicate m mat
         vecList = V.replicate m list
         listList = L.replicate m list
         listVector = L.replicate m vec
     --print . V.foldl1' (V.zipWith (+)) $ vecs
     --print . V.foldl1' (VU.zipWith (+)) $ vecsU
     --print . V.foldl1' (+) $ mats
     print . V.foldl1' (\(!a) !b -> L.zipWith (+) a b) $ vecList
     --print . L.foldl1' (\(!a) !b -> L.zipWith (+) a b) $ listList
     --print . L.foldl1' (\(!a) !b -> V.zipWith (+) a b) $ listVector
     --print . V.foldl1' (VU.zipWith (+)) $ vecVecU
     --print . L.foldl1' (VU.zipWith (+)) $ listVecU
     --print . V.foldl1' (VU.zipWith (+)) . V.map crossProduct $ vecVecU
