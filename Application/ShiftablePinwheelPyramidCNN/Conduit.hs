module Application.ShiftablePinwheelPyramidCNN.Conduit
  ( module RC
  , module Application.ShiftablePinwheelPyramidCNN.Conduit
  , module Application.CaffeData.HDF5
  ) where

import           Application.CaffeData.HDF5 
import           Application.RotatedMNIST.Conduit      as RC
import           Control.Monad                         as M
import           Control.Monad.IO.Class
import           Control.Monad.Parallel                as MP
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.ShiftablePinwheelPyramidCNN
import           CV.Utility.DFT                        as DFT
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility           (arrayToUnboxed)
import           CV.Utility.Time
import           Data.Array.Repa                       as R
import           Data.Binary
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.List                     as CL
import           Data.List                             as L
import           Data.Vector.Storable                  as VS
import           Data.Vector.Unboxed                   as VU
import           Foreign.C.String
import           Foreign.Marshal.Array
import           Foreign.Ptr

shiftablePinwheelBlobPyramidCNNConduit
  :: ParallelParams
  -> DFTPlan
  -> ShiftablePinwheelBlobPyramidFilters
  -> Conduit (Double, ShiftablePinwheelPyramidInputArray) (ResourceT IO) [LabeledArray DIM3 Double]
shiftablePinwheelBlobPyramidCNNConduit parallelParams plan filter' = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do ys <-
          liftIO $
          MP.mapM
            (\(label, arr) -> do
               zs <- shiftablePinwheelBlobPyramidCNN plan filter' arr
               return . L.map (L.map (LabeledArray (round label))) $ zs)
            xs
        sourceList . L.concat $ ys
        shiftablePinwheelBlobPyramidCNNConduit parallelParams plan filter')

shiftablePinwheelBlobPyramidScatteringNetworkCNNConduit
  :: ParallelParams
  -> DFTPlan
  -> ShiftablePinwheelBlobPyramidScatteringNetworksFilters
  -> Int
  -> Int
  -> Int
  -> Conduit (Double, ShiftablePinwheelPyramidInputArray) (ResourceT IO) [LabeledArray DIM3 Double]
shiftablePinwheelBlobPyramidScatteringNetworkCNNConduit parallelParams plan filters m numLayers k = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do ys <-
          liftIO $
          MP.mapM
            (\(label, arr) -> do
               zs <-
                 shiftablePinwheelBlobPyramidScatteringNetworksCNN
                   plan
                   filters
                   m
                   numLayers
                   k
                   [arr]
               return . featureArray2LabeledArrayList (round label) $ zs)
            xs
        sourceList . L.concat $ ys
        shiftablePinwheelBlobPyramidScatteringNetworkCNNConduit
          parallelParams
          plan
          filters
          m
          numLayers
          k)
