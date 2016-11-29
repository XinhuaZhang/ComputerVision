module CV.CUDA.DataType where

-- import           Data.Array.Accelerate              as A
-- import           Data.Array.Accelerate.CUDA
-- import           Data.Array.Accelerate.Data.Complex as A
import           Data.Array.Unboxed                 as AU
import           GHC.Float
import           Prelude                            as P

data GPUDataType
  = GPUFloat
  | GPUDouble
  deriving (Show, Read)

-- data ComputeMode
--   = GPU GPUDataType
--         [Context]
--   | CPU

-- data AccComplexArray
--   = GPUFloatComplexArr (Acc (A.Array DIM3 (A.Complex Float)))
--   | GPUDoubleComplexArr (Acc (A.Array DIM3 (A.Complex Double)))

-- data CPUArray
--   = CPUFloatArr (AU.Array (Int, Int, Int) Float)
--   | CPUDoubleArr (AU.Array (Int, Int, Int) Double)

-- Arrfloat2double :: CPUArray -> CPUArray
-- arrFloat2Double (CPUFloatArr arr) =
--   CPUDoubleArr $
--   AU.array (bounds arr) . P.map (\(i, v) -> (i, float2Double v)) . assocs $ arr
  
-- arrDouble2Float :: CPUArray -> CPUArray
-- arrDouble2Float (CPUDoubleArr arr) =
--   CPUFloatArr $
--   AU.array (bounds arr) . P.map (\(i, v) -> (i, double2Float v)) . assocs $ arr
