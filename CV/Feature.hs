{-# LANGUAGE TypeFamilies #-}
module CV.Feature where

class Feature a  where
  type Similarity a :: *
  similarity :: a -> a -> Similarity a
