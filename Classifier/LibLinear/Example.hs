module Classifier.LibLinear.Example where

import           Control.Monad
import           Control.Monad.IO.Class                        (liftIO)
import           Data.Conduit
import           Data.Conduit.List                             as CL
import           Foreign.C
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Classifier.LibLinear.Bindings
import           Classifier.LibLinear.Solver
import           Prelude                                       as P

data LibLinearFeature = Dense [Double] | Sparse [Double]

readLabelFile :: FilePath -> IO [Double]
readLabelFile filePath =
  do bs <- readFile filePath
     return . P.map (\x -> read x :: Double) . lines $! bs

labelSource :: FilePath -> Source IO Double
labelSource filePath =
  do labels <- liftIO . readLabelFile $ filePath
     sourceList labels

newParameter :: Solver -> Double -> C'parameter
newParameter solver c = C'parameter
  { c'parameter'solver_type = fromIntegral $ fromEnum solver
  , c'parameter'eps = 0.1
  , c'parameter'C = realToFrac c
  , c'parameter'nr_weight = 0
  , c'parameter'weight_label = nullPtr
  , c'parameter'weight = nullPtr
  , c'parameter'p = 0.1
  , c'parameter'init_sol = nullPtr
  }

{-# INLINE getFeature #-}

getFeature :: LibLinearFeature -> [C'feature_node]
getFeature (Dense xs) = pairs ++ [C'feature_node (-1) 0]
  where
    pairs =
      P.zipWith
        (\i x -> C'feature_node i (realToFrac x))
        [1 ..]
        xs
getFeature (Sparse xs) = (pairs ++ [C'feature_node (-1) 0])
  where
    pairs =
      P.map (\(i, x) -> C'feature_node i (realToFrac x)) .
      P.filter (\(_i, x) -> x /= 0) . P.zip [1 ..] $
      xs


getFeatureVecPtr :: LibLinearFeature -> IO (Ptr C'feature_node)
getFeatureVecPtr (Dense xs) =
  newArray (pairs ++
            [C'feature_node (-1)
                            0])
  where pairs =
          P.zipWith (\i x ->
                       C'feature_node (P.fromIntegral i)
                                      (realToFrac $ x))
                    [1 ..]
                    xs
        max = P.maximum xs
getFeatureVecPtr (Sparse xs) =
  newArray (pairs ++
            [C'feature_node (-1)
                            0])
  where pairs =
          P.map (\(i,x) ->
                   C'feature_node (P.fromIntegral i)
                                  (realToFrac $ (x))) .
          P.filter (\(i,x) -> x /= 0) . P.zip [1 ..] $
          xs
        max = P.maximum xs


getFeatureArrPtr
  :: [LibLinearFeature] -> IO (Ptr (Ptr C'feature_node))
getFeatureArrPtr xs =
  do ys <- P.mapM getFeatureVecPtr xs
     arrPtr <- newArray ys
     return arrPtr

getLabelVecPtr :: [Double] -> IO (Ptr CDouble)
getLabelVecPtr = newArray . P.map realToFrac
