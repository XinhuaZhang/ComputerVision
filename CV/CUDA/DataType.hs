module CV.CUDA.DataType where

import           Data.Array.Accelerate as A

data GPUDataType
  = GPUFloat
  | GPUDouble
  deriving (Show)
