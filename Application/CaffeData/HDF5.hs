{-# LANGUAGE FlexibleContexts #-}
module Application.CaffeData.HDF5 where

import           Application.CaffeData.HDF5Bindings
import           Control.Arrow
import           Control.Monad                      as M
import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Utility.Parallel
import           CV.Utility.Utility
import           Data.Array.Repa                    as R
import           Data.Conduit                       as C
import           Data.Conduit.List                  as CL
import           Data.List                          as L
import           Data.Vector.Storable               as VS
import           Data.Vector.Unboxed                as VU
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal
import           Foreign.Ptr
import           Foreign.Storable                   as FS (peek)
import           GHC.Float
import           System.Directory
import           System.FilePath
import           System.IO
import           Text.Printf


hdf5Sink :: ParallelParams
         -> String
         -> Sink [LabeledArray DIM3 Double] (ResourceT IO) ()
hdf5Sink parallelParams folderName = do
  x <- CL.peek
  case x of
    Nothing -> return ()
    Just y -> do
      liftIO $ M.mapM_ (print . extent . (\(LabeledArray _ arr) -> arr)) y
      currentPath <- liftIO $ getCurrentDirectory
      let filePath = currentPath </> folderName
      liftIO $ removePathForcibly filePath
      liftIO $ createDirectoryIfMissing True filePath
      handle <- liftIO $ openFile (filePath </> "fileList.txt") WriteMode
      loop filePath 0 handle
  where
    loop filePath count h = do
      xs <- CL.take (batchSize parallelParams)
      if L.null xs
        then liftIO $ hClose h
        else do
          let (labels, ys) =
                first L.head .
                L.unzip .
                L.map
                  (second ((L.head *** VU.concat) . L.unzip) .
                   L.unzip .
                   parMap
                     rdeepseq
                     (\(LabeledArray label arr) ->
                        ( label
                        , ( L.map fromIntegral .
                            L.reverse . listOfShape . extent $
                            arr
                          , rescaleUnboxedVector (0, 1) . toUnboxed $ arr)))) .
                L.transpose $
                xs
              path = filePath </> show count <.> "h5"
          liftIO $ hPutStrLn h path
          liftIO $ hFlush h
          cPath <- liftIO $ newCString path
          hid <- liftIO $ c'H5Fcreate cPath h5f_acc_excl h5p_default h5p_default
          when (hid < 0) (error "c'H5Fcreate")
          labelStr <- liftIO $ newCString $ "label"
          status <-
            liftIO $
            withArray [fromIntegral . L.length $ labels, 1] $ \dims ->
              withArray (L.map (CFloat . fromIntegral) labels) $ \buffer ->
                c'H5LTmake_dataset_float hid labelStr 2 dims buffer
          when (status < 0) (error "c'H5LTmake_dataset_float: label")
          liftIO $
            M.zipWithM_
              (\i (shapeList, y) -> do
                 dataStr <- newCString $ "data" L.++ show i
                 status <-
                   withArray ((fromIntegral . L.length $ xs) : shapeList) $ \dims ->
                     unsafeWith
                       (VS.map (CFloat . double2Float) . VS.convert $ y) $ \buffer ->
                       c'H5LTmake_dataset_float hid dataStr 4 dims buffer
                 when (status < 0) (error "c'H5LTmake_dataset_float: data"))
              [0 :: Int ..]
              ys
          status <- liftIO $ c'H5Fclose hid
          when (status < 0) (error "c'H5Fclose")
          loop filePath (count + 1) h


hdf5Source
  :: FilePath
  -> String
  -> String
  -> C.Source (ResourceT IO) (LabeledArray DIM3 Double)
hdf5Source filePath labelName dataName = do
  h <- liftIO $ openFile filePath ReadMode
  labelStr <- liftIO $ newCString labelName
  dataStr <- liftIO $ newCString dataName
  loop h labelStr dataStr
  where
    loop h labelStr dataStr = do
      flag <- liftIO $ hIsEOF h
      if flag
        then liftIO $ hClose h
        else do
          path <- liftIO $ hGetLine h
          hid <-
            liftIO $
            withCString path $ \cPath ->
              c'H5Fopen cPath h5f_acc_rdonly h5p_default
          when (hid < 0) (error "c'H5Fopen")
          label <-
            liftIO $
            alloca
              (\p -> do
                 status <- c'H5LTread_dataset_float hid labelStr p
                 when (status < 0) (error "c'H5LTread_dataset_float: label")
                 FS.peek p)
          ndims <-
            liftIO $
            fmap fromIntegral . alloca $ \p -> do
              status <- c'H5LTget_dataset_ndims hid dataStr p
              when (status < 0) (error "c'H5LTget_dataset_ndims")
              FS.peek p
          when
            (ndims /= 4)
            (error "hdf5Source: hdf5 file is not a 4-dimension array.")
          dims <-
            liftIO $
            fmap (L.map fromIntegral) . alloca $ \classP ->
              alloca $ \typeSizeP ->
                allocaArray ndims $ \arrP -> do
                  status <-
                    c'H5LTget_dataset_info hid dataStr arrP classP typeSizeP
                  when (status < 0) (error "c'H5LTget_dataset_info")
                  peekArray ndims arrP
          arrs <-
            liftIO $
            fmap
              (\xs ->
                 let arr =
                       fromListUnboxed (shapeOfList . L.reverse $ dims) .
                       L.map (float2Double . (\(CFloat x) -> x)) $
                       xs
                 in L.map
                      (\i ->
                         computeS . R.slice arr $ (Z :. i :. All :. All :. All))
                      [0 .. (L.last dims) - 1]) .
            allocaArray (L.product dims) $ \arrP -> do
              status <- c'H5LTread_dataset_float hid dataStr arrP
              when (status < 0) (error "c'H5LTread_dataset_float")
              peekArray (L.product dims) arrP
          status <- liftIO $ c'H5Fclose hid
          when (status < 0) (error "c'H5Fclose")
          sourceList . L.map (LabeledArray (round label)) $ arrs
          loop h labelStr dataStr
