module CV.CUDA.Context where

import           Control.Parallel.Strategies
import           Data.Array.Accelerate       as A
import           Data.Array.Accelerate.CUDA  as A
import           Foreign.CUDA.Driver         as CUDA
import           Prelude                     as P

data GPUDevice
  = ALL
  | Option [Int]
  deriving (Show)

initializeGPUCtx :: GPUDevice -> IO [A.Context]
initializeGPUCtx ALL = do
  CUDA.initialise []
  num <- CUDA.count
  putStrLn ("There are " P.++ show num P.++ " GPUS.")
  if num > 0
    then do
      dev <- P.mapM CUDA.device [0 .. (num - 1)]
      ctx <- P.mapM (\d -> A.create d []) dev
      return ctx
    else error "There is no GPU device on this machine."
initializeGPUCtx (Option []) = error "You should specify the id of GPU."
initializeGPUCtx (Option devID) = do
  CUDA.initialise []
  dev <- P.mapM CUDA.device devID
  ctx <- P.mapM (\d -> A.create d []) dev
  return ctx

destoryGPUCtx :: [A.Context] -> IO ()
destoryGPUCtx = mapM_ A.destroy

performGCCtx :: [A.Context] -> IO ()
performGCCtx = mapM_ performGCIn 

multiGPUStream
  :: (Arrays a, Arrays b)
  => [A.Context] -> (Acc a -> Acc b) -> [a] -> [b]
multiGPUStream ctx kernel xs =
  P.concat .
  parMap rseq (\(x, y) -> streamIn x kernel y) .
  P.zip ctx . splitList devLen (div dataLen devLen) $
  xs
  where
    dataLen = P.length xs
    devLen = P.length ctx
    splitList :: Int -> Int -> [a] -> [[a]]
    splitList _ _ [] = []
    splitList m n xs
      | m == 1 = [xs]
      | otherwise = as : splitList (m - 1) n bs
      where
        (as, bs) = P.splitAt n xs

multiGPUStreamList
  :: (Arrays a, Arrays b)
  => [A.Context] -> (Acc a -> Acc b) -> [[a]] -> [[b]]
multiGPUStreamList ctx kernel xs
  | (P.length ctx) /= (P.length xs) =
    error "The number of lists and that of contexts are not equivalent."
  | otherwise = parMap rseq (\(x, y) -> streamIn x kernel y) $ P.zip ctx xs
