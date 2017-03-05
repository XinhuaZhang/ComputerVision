{-# LANGUAGE FlexibleContexts #-}
module Application.FacialExpression.Conduit where

import           Codec.Picture
import           Classifier.LibLinear
import           Foreign.Ptr
import           Control.Monad                  as M
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           CV.Filter.PolarSeparableFilter
import           CV.Utility.Parallel
import           Data.Array.Repa                as R
import           Data.Complex                   as C
import           Data.Conduit                   as C
import           Data.Conduit.List              as CL
import           Data.List                      as L
import           Data.Vector.Unboxed            as VU
import           Foreign.Marshal.Array

labelSource' :: FilePath -> C.Source (ResourceT IO) Int
labelSource' filePath = do
  xs <- liftIO . readFile $ filePath
  let ys = L.map (\x -> read x :: (String, Int)) . L.lines $ xs
  sourceList . snd . L.unzip $ ys


filePathSource :: FilePath -> C.Source (ResourceT IO) FilePath
filePathSource filePath = do
  xs <- liftIO . readFile $ filePath
  let ys = L.map (\x -> read x :: (String, Int)) . L.lines $ xs
  sourceList . fst . L.unzip $ ys


applyFilterCenterVariedSizeConduit
  :: (R.Source s Double)
  => ParallelParams
  -> PolarSeparableFilterParamsSet
  -> Conduit (R.Array s DIM3 Double) (ResourceT IO) (VU.Vector (Complex Double))
applyFilterCenterVariedSizeConduit parallelParams params@(PolarSeparableFilterParamsSet _ downSampleFactor scaleSet rfSet afSet name) = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\x ->
                    let (Z :. nf' :. ny' :. nx') = extent x
                        (Z :. nfFilter :. _ :. _) = extent filterArr
                        paramsSet =
                          PolarSeparableFilterParamsSet
                            (ny', nx')
                            downSampleFactor
                            scaleSet
                            rfSet
                            afSet
                            name
                        (PolarSeparableFilter _ filterArr) =
                          makeCenterFilterSet paramsSet
                    in toUnboxed .
                       sumS .
                       sumS .
                       traverse2
                         filterArr
                         x
                         (\(Z :. nf1 :. _ :. _) (Z :. nf2 :. _ :. _) ->
                             (Z :. nf1 * nf2 :. ny' :. nx')) $
                       \fFilter fImg (Z :. k :. j :. i) ->
                          let kImg = mod k nfFilter
                              kFilter = div k nfFilter
                          in ((:+ 0) . fImg $ (Z :. kImg :. j :. i)) *
                             fFilter (Z :. kFilter :. j :. i))
                xs
        sourceList ys
        applyFilterCenterVariedSizeConduit parallelParams params)


{-# INLINE complexVec2RealVec #-}

complexVec2RealVec :: VU.Vector (Complex Double) -> VU.Vector Double
complexVec2RealVec vec = a VU.++ b
  where
    (a, b) = VU.unzip . VU.map (\(x :+ y) -> (x, y)) $ vec

{-# INLINE complexVec2PhaseVec #-}

complexVec2PhaseVec :: VU.Vector (Complex Double) -> VU.Vector Double
complexVec2PhaseVec = VU.map phase

{-# INLINE makeCenterFilterSet #-}

makeCenterFilterSet
  :: PolarSeparableFilterParamsSet
  -> PolarSeparableFilter PolarSeparableFilterParamsSet (R.Array U DIM3 (C.Complex Double))
makeCenterFilterSet params@(PolarSeparableFilterParamsSet (ny, nx) _downSampleFactor scaleSet rfSet afSet _name) =
  PolarSeparableFilter params $
  R.fromListUnboxed (Z :. getFilterNum params :. ny :. nx) filterEleList
  where
    paramsList = generateParamsSet scaleSet rfSet afSet
    filterEleList =
      L.concatMap
        (\(scale, rf, af) ->
            makeCenterFilterList ny nx (getFilterSetFunc params scale rf af))
        paramsList


{-# INLINE makeCenterFilterList #-}

makeCenterFilterList :: Int -> Int -> (Int -> Int -> a) -> [a]
makeCenterFilterList ny nx f =
  [ f (r - div ny 2) (c - div nx 2)
  | r <- [0 .. ny - 1]
  , c <- [0 .. nx - 1] ]


{-# INLINE featurePtrConduit #-}

featurePtrConduit :: Conduit (VU.Vector Double) (ResourceT IO) (Ptr C'feature_node)
featurePtrConduit =
  awaitForever
    (\vec -> do
       featurePtr <- liftIO $ newArray . getFeature . Dense . VU.toList $ vec
       yield featurePtr)
