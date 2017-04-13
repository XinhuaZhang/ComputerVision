{-# LANGUAGE FlexibleContexts #-}
import           Application.RecenterImage.Conduit
import           Control.Monad                     as M
import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           CV.Filter.GaussianFilter
import           CV.IO.ImageIO
import           CV.Utility.Parallel
import           Data.Array.Repa                   as R
import           Data.Array.Unboxed
import           Data.Conduit
import           Data.Conduit.Binary               as CB
import           Data.Conduit.List                 as CL
import           Data.Ix
import           Data.List                         as L
import           Data.Maybe
import           Data.Ord
import           Data.Vector.Unboxed               as VU
import           System.Environment



main = do
  (path:isColorStr:_) <- getArgs
  let parallelParams = ParallelParams {numThread = 1, batchSize = 1}
  imgs <-
    runResourceT $
    CL.sourceList [path] $$ readImageConduit (read isColorStr :: Bool) =$=
    findCenterConduit parallelParams =$=
    CL.take 1
  M.zipWithM_ (\i img -> plotImage (show i L.++ ".png") img) [1 ..] imgs
