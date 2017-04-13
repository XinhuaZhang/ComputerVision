{-# LANGUAGE FlexibleContexts #-}
import           Application.RecenterImage.Conduit
import           Control.Monad                     as M
import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           CV.Array.LabeledArray
import           CV.Filter.GaussianFilter
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

extractConduit
  :: Int
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (LabeledArray DIM3 Double)
extractConduit n =
  awaitForever
    (\(LabeledArray l arr) -> when (l == n) (yield (LabeledArray l arr)))

main = do
  let path =
        "/home/xzhang/Workspaces/ComputerVision/Application/Leaf/Train_128_0_Color.dat"
      gaussianParams = GaussianFilterParams 32 128 128
      parallelParams = ParallelParams {numThread = 1, batchSize = 1}
      gaussianFilter = makeFilter gaussianParams
      labelIndex = 2
  recenteredImgs <-
    runResourceT $
    CB.sourceFile path $$ readLabeledImagebinaryConduit =$=
    extractConduit labelIndex =$=
    recenterFixedSizeConduit parallelParams gaussianFilter =$=
    CL.map (\(LabeledArray _ arr) -> arr) =$=
    CL.map
      (\arr ->
         computeUnboxedS $
         R.traverse
           arr
           id
           (\f idx@(Z :. k :. j :. i) ->
              if i == 128 || j == 128
                then 127
                else f idx)) =$=
    CL.take 1
  imgs <-
    runResourceT $
    CB.sourceFile path $$ readLabeledImagebinaryConduit =$=
    extractConduit labelIndex =$=
    CL.map (\(LabeledArray _ arr) -> arr) =$=
    CL.map
      (\arr ->
         computeUnboxedS $
         R.traverse
           arr
           id
           (\f idx@(Z :. k :. j :. i) ->
              if i == 64 || j == 64
                then 127
                else f idx)) =$=
    CL.take 1
  M.zipWithM_
    (\i img -> plotImage (show i L.++ ".png") img)
    [1 ..]
    imgs
  M.zipWithM_
    (\i img -> plotImage ("Recentered_" L.++ show i L.++ ".png") img)
    [1 ..]
    recenteredImgs 
