{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PatternGuards              #-}
import           Application.Video.IO
import           Application.Video.Opencv.Bindings
import           Codec.Picture
import           Control.Arrow
import           Control.Monad                     as M
import           Control.Monad.Trans.Resource
import           CV.Filter.PolarSeparableFilter
import           CV.Utility.RepaArrayUtility
import           Data.Array                        as Arr
import           Data.Array.Repa                   as R
import           Data.Complex                      as C
import           Data.Conduit
import           Data.Conduit.List                 as CL
import           Data.Image
import           Data.List                         as L
import           Data.Set                          as S
import           Data.Vector.Storable              as VS
import           Foreign.C.Types
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           GHC.Float
import Data.Vector as V


main =
  do frames <-
       runResourceT $ videoFrameSource "v_Biking_g01_c01.avi" $$ CL.take 2
     M.zipWithM_ (\i frame -> plotFrame (show i L.++ ".png") frame)
                 [1 ..]
                 frames
     let w = imageWidth . (\(ImageY8 img) -> img) . L.head $ frames
         h = imageHeight . (\(ImageY8 img) -> img) . L.head $ frames
         frame0 =
           VS.map CUChar . imageData . (\(ImageY8 img) -> img) . L.head $
           frames
         frame1 =
           VS.map CUChar . imageData . (\(ImageY8 img) -> img) . L.last $
           frames
     buf <- mallocArray (w * h * 2) :: IO (Ptr CFloat)
     unsafeWith frame0 $
       \f0 ->
         unsafeWith frame1 $
         \f1 ->
           c'computeOpticalFlow (fromIntegral h)
                                (fromIntegral w)
                                f0
                                f1
                                buf
     result <- peekArray (w * h * 2) buf
     let pariList = parsePair result
         xs =
           L.map round . norm . L.map (\(CFloat x) -> x) . fst . L.unzip $
           pariList
         vx = ImageY8 . Image w h . VS.fromList $ xs
         ys =
           L.map round . norm . L.map (\(CFloat x) -> x) . snd . L.unzip $
           pariList
         vy = ImageY8 . Image w h . VS.fromList $ ys
     plotFrame ("vx.png") vx
     plotFrame ("vy.png") vy
     let frameArr0 =
           fromUnboxed (Z :. 1 :. h :. w) .
           VS.convert .
           VS.map fromIntegral . imageData . (\(ImageY8 img) -> img) . L.head $
           frames :: R.Array U DIM3 Double
         frameArr1 =
           fromUnboxed (Z :. 1 :. h :. w) .
           VS.convert .
           VS.map fromIntegral . imageData . (\(ImageY8 img) -> img) . L.last $
           frames :: R.Array U DIM3 Double
         filterParamsSet =
           PolarSeparableFilterParamsSet {getSizeSet = (h,w)
                                         ,getDownsampleFactorSet = 1
                                         ,getScaleSet =
                                            S.fromDistinctAscList [1]
                                         ,getRadialFreqSet =
                                            S.fromDistinctAscList [0 .. (4 - 1)]
                                         ,getAngularFreqSet =
                                            S.fromDistinctAscList [0 .. (4 - 1)]
                                         ,getNameSet = Pinwheels}
         frameComplex0 =
           applyFilterSetVariedSize filterParamsSet . R.map (:+ 0) $ frameArr0
         frameComplex1 =
           applyFilterSetVariedSize filterParamsSet . R.map (:+ 0) $ frameArr1
         flow =
           fromListUnboxed (Z :. 1 :. h :. w) .
           L.map (join (***) (\(CFloat x) -> float2Double x)) $
           pariList :: R.Array U DIM3 (Double,Double)
         !ds =
           V.generate
             16
             (\i ->
                computeComplexDerivativeS . computeS . R.slice frameComplex1 $
                (Z :. i :. All :. All))
     angularFlow <-
       (computeP . traverse2 frameComplex0 flow const $
        \fFrame0 fFlow idx@(Z :. k :. j :. i) ->
          let (fx,fy) = fFlow (Z :. 0 :. j :. i)
              newJ = fromIntegral j + fy
              newI = fromIntegral i + fx
          in if newI < 0 ||
                newI > fromIntegral w - 1 ||
                newJ < 0 || newJ > fromIntegral h - 1
                then 0 :+ 0
                else (bicubicInterpolationComplex (ds V.! k)
                                                  (newJ,newI)) -
                     fFrame0 idx) :: IO (R.Array U DIM3 (C.Complex Double))
     let angularFlowList =
           L.map (\i ->
                    listArray ((0,0),(h - 1,w - 1)) .
                    R.toList . R.slice angularFlow $
                    (Z :. (i :: Int) :. All :. All))
                 [0 .. 15]
     M.zipWithM_
       (\i af ->
          writeImage ("angularFlowComplex" L.++ show i L.++ ".ppm")
                     (arrayToImage af :: ComplexImage))
       [1 ..]
       angularFlowList
     M.zipWithM_
       (\i af ->
          writeImage ("angularFlowMagnitude" L.++ show i L.++ ".pgm")
                     (imageMap C.magnitude (arrayToImage af :: ComplexImage) :: GrayImage))
       [1 ..]
       angularFlowList

parsePair :: [a] -> [(a,a)]
parsePair (x: [])  = error "parsePair: the length of the list is not even."
parsePair []       = []
parsePair (x:y:xs) = (x,y) : parsePair xs

norm :: (Fractional a
        ,Ord a
        ,Num a)
     => [a] -> [a]
norm xs = L.map (\x -> (x - min') / (max' - min') * 255) xs
  where max' = L.maximum xs
        min' = L.minimum xs
