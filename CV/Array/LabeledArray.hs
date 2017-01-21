{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs  #-}
module CV.Array.LabeledArray where

import           Control.Monad                as M
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           Data.Array.Repa              as R
import           Data.Binary
import           Data.ByteString              as BS
import           Data.ByteString.Lazy         as BL
import           Data.Conduit                 as C
import           Data.Conduit.Binary          as CB
import           Data.Conduit.List            as CL
import           Data.Vector.Unboxed          as VU
import           GHC.Generics
import           Prelude                      as P
import           System.IO
import           CV.Utility.Parallel           

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
      
readLabeledImagebinary :: FilePath -> IO [LabeledArray DIM3 Double]
readLabeledImagebinary filePath =
  withBinaryFile
    filePath
    ReadMode
    (\h -> do
       lenBS <- BL.hGet h 4
       let len = fromIntegral (decode lenBS :: Word32) :: Int
       M.replicateM
         len
         (do sizeBS <- BL.hGet h 4
             let size' = fromIntegral (decode sizeBS :: Word32) :: Int
             bs <- BL.hGet h size'
             let (LabeledArray label arr) = decode bs :: LabeledArray DIM3 Word8
             return . LabeledArray label . computeUnboxedS . rescaleWord8 $
               arr))


readLabeledImagebinarySource :: FilePath -> C.Source IO (LabeledArray DIM3 Double)
readLabeledImagebinarySource filePath = do
  h <- liftIO $ openBinaryFile filePath ReadMode
  lenBS <- liftIO $ BL.hGet h 4
  let len = fromIntegral (decode lenBS :: Word32) :: Int
  CL.unfoldM
    (\(handle, count') ->
        if count' < len
          then do
            sizeBS <- liftIO $ BL.hGet h 4
            let size' = fromIntegral (decode sizeBS :: Word32) :: Int
            bs <- BL.hGet h size'
            if fromIntegral (BL.length bs) < size'
              then error $
                   "Expect " P.++ show size' P.++ " images, but only have " P.++
                   show count' P.++
                   "."
              else let (LabeledArray label arr) =
                         decode bs :: LabeledArray DIM3 Word8
                   in return $
                      Just
                        ( LabeledArray label . computeUnboxedS . R.map fromIntegral $
                          arr
                        , (handle, count' + 1))
          else do
            isEoF <- hIsEOF handle
            hClose handle
            if isEoF
              then return Nothing
              else error $
                   "Expect " P.++ show len P.++
                   " images, but there are more images in the file. ")
    (h, 0)


readLabeledImagebinaryConduit :: Conduit BS.ByteString (ResourceT IO) (LabeledArray DIM3 Double)
readLabeledImagebinaryConduit = do
  CB.drop 4
  go
  where
    go = do
      sizeBS <- CB.take 4
      when
        (BL.length sizeBS > 0)
        (do let size' = fromIntegral (decode sizeBS :: Word32) :: Int
            bs <- CB.take size'
            let (LabeledArray label arr) = decode bs :: LabeledArray DIM3 Word8
            yield . LabeledArray label . computeUnboxedS . rescaleWord8 $
              arr
            go)
            

readLabeledImageBinary :: FilePath -> Int -> IO [LabeledArray DIM3 Double]
readLabeledImageBinary filePath num =
  withBinaryFile filePath ReadMode $
  \h -> do
    lenBS <- BL.hGet h 4
    let len = fromIntegral (decode lenBS :: Word32) :: Int
        len1 = min len num
    M.replicateM len1 $
      do sizeBS <- BL.hGet h 4
         let size' = fromIntegral (decode sizeBS :: Word32) :: Int
         bs <- BL.hGet h size'
         let (LabeledArray label arr) = decode bs :: LabeledArray DIM3 Word8
         return . LabeledArray label . computeUnboxedS . rescaleWord8 $ arr


writeLabeledImageBinarySink :: FilePath
                            -> Int
                            -> Sink (LabeledArray DIM3 Double) IO ()
writeLabeledImageBinarySink filePath len = do
  h <- liftIO $ openBinaryFile filePath WriteMode
  liftIO $ BL.hPut h (encode (fromIntegral len :: Word32))
  CL.foldMapM
    (\x ->
        let encodedX = encodeLabeledImg x
            len' = fromIntegral . BL.length $ encodedX :: Word32
        in do BL.hPut h . encode $ len'
              BL.hPut h encodedX)
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


meanSubtractConduit
  :: ParallelParams
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (LabeledArray DIM3 Double)
meanSubtractConduit parallelParams = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (P.null xs)
    (do let !ys =
              parMapChunk
                parallelParams
                rseq
                (\(LabeledArray label arr) ->
                    let !m = sumAllS arr / fromIntegral (nf * nx * ny)
                        (Z :. nf :. ny :. nx) = extent arr
                        !result = computeUnboxedS $ R.map (\x -> x - m) arr
                    in deepSeqArray result (LabeledArray label result))
                xs
        sourceList ys
        meanSubtractConduit parallelParams)
-- awaitForever
--   (\(LabeledArray label arr) ->
--       let !m = sumAllS arr / fromIntegral (nf * nx * ny)
--           (Z :. nf :. ny :. nx) = extent arr
--       in yield $! LabeledArray label . computeUnboxedS $ R.map (\x -> x - m) arr)

-- rescaleWord8 [0,255] to [-1,1]

{-# INLINE rescaleWord8 #-}

rescaleWord8 :: (R.Source s Word8) => R.Array s DIM3 Word8 -> R.Array R.D DIM3 Double
rescaleWord8 = R.map (\x -> ((fromIntegral x / 255) - 0.5) * 2)
