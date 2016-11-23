{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs  #-}
module CV.Array.LabeledArray where

import           Control.Monad          as M
import           Control.Monad.IO.Class (liftIO)
import           CV.Array.Image
import           Data.Array.Repa        as R
import           Data.Binary
import           Data.ByteString.Lazy   as BL
import           Data.Conduit           as C
import           Data.Conduit.List      as CL
import           Data.Vector.Unboxed    as VU
import           GHC.Generics
import           Prelude                as P
import           System.IO

data LabeledArray sh e =
  LabeledArray !Int
               (Array U sh e)
  deriving (Show,Generic)

instance (Binary e, Unbox e, Shape sh) =>
         Binary (LabeledArray sh e) where
  put (LabeledArray label arr) = do
    put label
    put . listOfShape . extent $ arr
    put . R.toList $ arr
  get = do
    label' <- get
    shList <- get
    elemList <- get
    return $!
      LabeledArray label' (fromListUnboxed (shapeOfList shList) elemList)


readLabeledImagebinarySource :: FilePath -> C.Source IO (LabeledArray DIM3 Double)
readLabeledImagebinarySource filePath = do
  h <- liftIO $ openBinaryFile filePath ReadMode
  lenBS <- liftIO $ BL.hGet h 4
  sizeBS <- liftIO $ BL.hGet h 4
  let len = fromIntegral (decode lenBS :: Word32) :: Int
      size = fromIntegral (decode sizeBS :: Word32) :: Int
  CL.unfoldM
    (\(handle, count) ->
        if count < len
          then do
            bs <- BL.hGet h size
            if fromIntegral (BL.length bs) < size
              then error $
                   "Expect " P.++ show size P.++ " images, but only have " P.++
                   show count P.++
                   "."
              else let (LabeledArray label arr) =
                         decode bs :: LabeledArray DIM3 Word8
                   in return $
                      Just
                        ( LabeledArray label . computeUnboxedS . R.map fromIntegral $
                          arr
                        , (handle, count + 1))
          else do
            isEoF <- hIsEOF handle
            hClose handle
            if isEoF
              then return Nothing
              else error $
                   "Expect " P.++ show size P.++
                   " images, but there are more images in the file. ")
    (h, 0)

writeLabeledImageBinarySink :: FilePath
                            -> Int
                            -> Sink (LabeledArray DIM3 Double) IO ()
writeLabeledImageBinarySink filePath len = do
  h <- liftIO $ openBinaryFile filePath WriteMode
  liftIO $ BL.hPut h (encode (fromIntegral len :: Word32))
  x <- await
  case x of
    Nothing -> return ()
    Just y ->
      let encodedY = encodeLabeledImg y
          len = fromIntegral . BL.length $ encodedY :: Word32
      in do liftIO . BL.hPut h . encode $ len
            liftIO $ BL.hPut h encodedY
  CL.foldMapM (BL.hPut h . encodeLabeledImg)
  liftIO $ hClose h
  where
    encodeLabeledImg (LabeledArray label arr) =
      encode .
      LabeledArray label .
      computeS .
      R.map (\x -> round x :: Word8) .
      normalizeImage (fromIntegral (maxBound :: Word8)) $
      arr

getArrayNumFile :: FilePath -> IO Int
getArrayNumFile filePath =
  withBinaryFile
    filePath
    ReadMode
    (\h -> do
       lenBS <- liftIO $ BL.hGet h 4
       let len = fromIntegral (decode lenBS :: Word32) :: Int
       return len)
