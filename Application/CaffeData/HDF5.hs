{-# LANGUAGE FlexibleContexts #-}
module Application.CaffeData.HDF5 where

import           Application.CaffeData.HDF5Bindings
import           Control.Monad                      as M
import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Parallel             as MP
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Utility.Parallel
import           Data.Array.Repa                    as R
import           Data.Conduit                       as C
import           Data.Conduit.List                  as CL
import           Data.List                          as L
import           Data.Vector.Storable               as VS
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal
import           Foreign.Ptr
import           Foreign.Storable                   as FS (peek)
import           GHC.Float
import           System.Directory
import           System.FilePath
import           System.IO


hdf5Sink :: ParallelParams
         -> String
         -> Sink [LabeledArray DIM3 Double] (ResourceT IO) ()
hdf5Sink parallelParams folderName = do
  x <- CL.peek
  case x of
    Nothing -> return ()
    Just y -> do
      liftIO $ removePathForcibly folderName
      liftIO $ createDirectory folderName
      currentPath <- liftIO $ getCurrentDirectory
      let filePath = currentPath </> folderName
      handles <-
        liftIO $
        M.mapM
          (\i -> do
             createDirectory (filePath </> show i)
             openFile (filePath </> show i </> "fileList.txt") WriteMode)
          [0 .. L.length y - 1]
      loop filePath 0 handles
      liftIO $ M.mapM_ hClose handles
  where
    loop filePath count hs = do
      xs <- CL.take (batchSize parallelParams)
      if L.null xs
        then liftIO $ M.mapM_ hClose hs
        else do
          let ys = L.zip hs . L.transpose $ xs
          liftIO $
            M.zipWithM_
              (\i (handle, y) -> do
                 M.zipWithM_
                   (\j (LabeledArray label arr) -> do
                      let path =
                            filePath </> show i </> show (j + count) <.> "h5"
                      hPutStrLn handle path
                      cPath <- newCString path
                      hid <-
                        c'H5Fcreate cPath h5f_acc_excl h5p_default h5p_default
                      when (hid < 0) (error "c'H5Fcreate")
                      dataStr <- newCString $ "data" L.++ show i
                      labelStr <- newCString $ "label" L.++ show i
                      status <-
                        withArray [1, 1] $ \dims ->
                          with (CFloat . fromIntegral $ label) $ \buffer ->
                            c'H5LTmake_dataset_float hid labelStr 2 dims buffer
                      when
                        (status < 0)
                        (error "c'H5LTmake_dataset_float: label")
                      status <-
                        withArray
                          (((:) 1) .
                           L.map fromIntegral . L.reverse . listOfShape . extent $
                           arr) $ \dims ->
                          unsafeWith
                            (VS.map (CFloat . double2Float) .
                             VS.convert . toUnboxed $
                             arr) $ \buffer ->
                            c'H5LTmake_dataset_float hid dataStr 4 dims buffer
                      when (status < 0) (error "c'H5LTmake_dataset_float: data")
                      status <- c'H5Fclose hid
                      when (status < 0) (error "c'H5Fclose"))
                   [0 .. L.length y - 1]
                   y)
              [0 .. L.length hs - 1]
              ys
          -- loop filePath (count + L.length xs) hs


hdf5Source :: FilePath -> C.Source (ResourceT IO) (LabeledArray DIM3 Double)
hdf5Source filePath = do
  h <- liftIO $ openFile filePath ReadMode
  labelStr <- liftIO $ newCString "label"
  dataStr <- liftIO $ newCString "data"
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
          dims <-
            liftIO $
            fmap (L.map fromIntegral) . alloca $ \classP ->
              alloca $ \typeSizeP ->
                allocaArray ndims $ \arrP -> do
                  status <-
                    c'H5LTget_dataset_info hid dataStr arrP classP typeSizeP
                  when (status < 0) (error "c'H5LTget_dataset_info")
                  peekArray ndims arrP
          arr <-
            liftIO $
            fmap
              (fromListUnboxed (shapeOfList . L.reverse $ dims) .
               L.map (float2Double . (\(CFloat x) -> x))) .
            allocaArray (L.product dims) $ \arrP -> do
              status <- c'H5LTread_dataset_float hid dataStr arrP
              when (status < 0) (error "c'H5LTread_dataset_float")
              peekArray (L.product dims) arrP
          status <- liftIO $ c'H5Fclose hid
          when (status < 0) (error "c'H5Fclose")
          yield (LabeledArray (round label) arr)
          loop h labelStr dataStr
