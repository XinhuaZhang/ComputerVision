module CV.CUDA.DataType where

import           Data.Array.Accelerate.CUDA

data GPUDataType
  = GPUFloat
  | GPUDouble
  deriving (Show)

data ComputeMode
  = GPU GPUDataType
        [Context]
  | CPU
