{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PatternGuards              #-}
import           Application.Video.IO
import           Application.Video.Opencv.Bindings
import           Codec.Picture
import           Control.Monad                     as M
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Conduit.List                 as CL
import           Data.List                         as L
import           Data.Vector.Storable              as VS
import           Foreign.C.Types
import           Foreign.Marshal.Array
import           Foreign.Ptr


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
     let xs =
           L.map round .
           normalize . L.map (\(CFloat x) -> x) . fst . L.unzip . parsePair $
           result
         vx = ImageY8 . Image w h . VS.fromList $ xs
         ys =
           L.map round .
           normalize . L.map (\(CFloat x) -> x) . snd . L.unzip . parsePair $
           result
         vy = ImageY8 . Image w h . VS.fromList $ ys
     plotFrame ("vx.png") vx
     plotFrame ("vy.png") vy
     



parsePair :: [a] -> [(a,a)]
parsePair (x: []) = error "parsePair: the length of the list is not even."
parsePair [] = []
parsePair (x:y:xs) = (x,y) : parsePair xs

normalize :: (Fractional a
             ,Ord a
             ,Num a)
          => [a] -> [a]
normalize xs = L.map (\x -> (x - min') / (max' - min') * 255) xs
  where max' = L.maximum xs
        min' = L.minimum xs
