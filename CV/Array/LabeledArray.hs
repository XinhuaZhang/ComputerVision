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
import Data.List as L
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility  as RAU
import           Control.DeepSeq

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
      
instance (Unbox e, Shape sh) =>
         NFData (LabeledArray sh e) where
  rnf (LabeledArray label arr) = seq label (deepSeqArray arr ())

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
             return . LabeledArray label . computeUnboxedS . R.map fromIntegral $
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
            yield . LabeledArray label . computeUnboxedS . R.map fromIntegral $
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
         return . LabeledArray label . computeUnboxedS . R.map fromIntegral $ arr


writeLabeledImageBinarySink :: FilePath
                            -> Int
                            -> Sink (LabeledArray DIM3 Double) (ResourceT IO) ()
writeLabeledImageBinarySink filePath len = do
  h <- liftIO $ openBinaryFile filePath WriteMode
  liftIO $ BL.hPut h (encode (fromIntegral len :: Word32))
  go h
  where
    encodeLabeledImg (LabeledArray label arr) =
      encode .
      LabeledArray label .
      computeS .
      R.map (\x -> round x :: Word8) .
      normalizeImage (fromIntegral (maxBound :: Word8)) $
      arr
    go handle = do
      x' <- await
      case x' of
        Nothing -> liftIO $ hClose handle
        Just x ->
          let encodedX = encodeLabeledImg x
              len' = fromIntegral . BL.length $ encodedX :: Word32
          in do liftIO . BL.hPut handle . encode $ len'
                liftIO $ BL.hPut handle encodedX
                go handle


getArrayNumFile :: FilePath -> IO Int
getArrayNumFile filePath =
  withBinaryFile
    filePath
    ReadMode
    (\h -> do
       lenBS <- liftIO $ BL.hGet h 4
       let len = fromIntegral (decode lenBS :: Word32) :: Int
       return len)


-- Set the maximum value of the maximum size, the ratio is intact.
resizeLabeledArrayConduit
  :: ParallelParams
  -> Int
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (LabeledArray DIM3 Double)
resizeLabeledArrayConduit parallelParams n = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rseq
                (\(LabeledArray l x) ->
                   let (Z :. nf' :. _ :. _) = extent x
                       (Z :. ny' :. nx') = extent . L.head $ zs
                       zs =
                         L.map
                           (\i ->
                              resize2DImageS n . R.slice x $
                              (Z :. i :. All :. All))
                           [0 .. nf' - 1]
                       arr =
                         fromUnboxed (Z :. nf' :. ny' :. nx') .
                         VU.concat . L.map toUnboxed $
                         zs
                   in (LabeledArray l (deepSeqArray arr arr)))
                xs
        sourceList ys
        resizeLabeledArrayConduit parallelParams n)


cropResizeLabeledArrayConduit
  :: ParallelParams
  -> Int
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (LabeledArray DIM3 Double)
cropResizeLabeledArrayConduit parallelParams n = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rseq
                (\(LabeledArray l x) ->
                   let (Z :. nf' :. ny' :. nx') = extent x
                       diff = div (abs $ ny' - nx') 2
                       y =
                         if ny' == nx'
                           then delay x
                           else if ny' > nx'
                                  then RAU.crop [0, diff, 0] [nx', nx', nf'] x
                                  else RAU.crop [diff, 0, 0] [ny', ny', nf'] x
                       zs =
                         L.map
                           (\i ->
                              resize2DImageS n . R.slice y $
                              (Z :. i :. All :. All))
                           [0 .. nf' - 1]
                       arr =
                         fromUnboxed (Z :. nf' :. n :. n) .
                         VU.concat . L.map toUnboxed $
                         zs
                   in (LabeledArray l (deepSeqArray arr arr)))
                xs
        sourceList ys
        cropResizeLabeledArrayConduit parallelParams n)

padResizeLabeledArrayConduit
  :: ParallelParams
  -> Int
  -> Double
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (LabeledArray DIM3 Double)
padResizeLabeledArrayConduit parallelParams n padVal = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rseq
                (\(LabeledArray l x) ->
                   let (Z :. nf' :. ny' :. nx') = extent x
                       maxSize = max ny' nx'
                       y = RAU.pad [maxSize, maxSize, nf'] padVal x
                       zs =
                         L.map
                           (\i ->
                              resize2DImageS n . R.slice y $
                              (Z :. i :. All :. All))
                           [0 .. nf' - 1]
                       arr =
                         fromUnboxed (Z :. nf' :. n :. n) .
                         VU.concat . L.map toUnboxed $
                         zs
                   in (LabeledArray l (deepSeqArray arr arr)))
                xs
        sourceList ys
        padResizeLabeledArrayConduit parallelParams n padVal)


padResizeRotateLabeledArrayConduit
  :: ParallelParams
  -> Int
  -> Double
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (LabeledArray DIM3 Double)
padResizeRotateLabeledArrayConduit parallelParams n deg = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rseq
                (\(LabeledArray label' arr) ->
                   let (Z :. nf :. _ny :. _nx) = extent arr
                   in L.map
                        (\x ->
                           let arr' =
                                 fromUnboxed (Z :. nf :. n :. n) .
                                 VU.concat . L.map R.toUnboxed $
                                 x
                           in deepSeqArray arr' (LabeledArray label' arr')) .
                      L.transpose .
                      L.map
                        (\i ->
                           padResizeRotate2DImageS n degs $
                           R.slice arr (Z :. i :. All :. All)) $
                      [0 .. nf - 1])
                xs
        sourceList . P.concat $ ys
        padResizeRotateLabeledArrayConduit parallelParams n deg)
  where
    !len =
      if deg == 0
        then 1
        else round (360 / deg) :: Int
    !degs = L.map (* deg) [0 .. fromIntegral len - 1]


padTransformImageConduit
  :: ParallelParams
  -> Double
  -> ImageTransformationParams
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (LabeledArray DIM3 Double)
padTransformImageConduit parallelParams padVal imageTransformationParams = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do params <-
          liftIO $
          M.replicateM
            (L.length xs)
            (generateImageTransformation imageTransformationParams)
        let ys =
              parZipWithChunk
                parallelParams
                rseq
                (\p (LabeledArray label' x) ->
                    L.map (LabeledArray label') $! padTransformImage padVal p x)
                params
                xs
        sourceList . L.concat $ ys
        padTransformImageConduit parallelParams padVal imageTransformationParams)
