module Application.ETH80.PathGenerator where

import           Control.Monad    as M
import           Data.List        as L
import           System.Directory
import           System.FilePath

getPaths :: FilePath -> IO [FilePath]
getPaths folderPath = do
  xs <- listDirectory folderPath
  return . L.map (folderPath </>) . L.filter (L.isSuffixOf ".png") $ xs

pathGenerator :: FilePath -> IO [[(Double,FilePath)]]
pathGenerator folderPath = do
  cars <-
    L.map (L.map ((,) 1)) <$>
    M.mapM
      (\i -> getPaths (folderPath </> "car" L.++ show i))
      [1 :: Int, 2, 3, 5, 6, 7, 9, 11, 12, 14]
  xs <-
    L.concat <$>
    M.zipWithM
      (\name i ->
          L.map (L.map ((,) i)) <$>
          M.mapM
            (\j -> getPaths (folderPath </> name L.++ show j))
            [(1 :: Int) .. 10])
      names
      [2 ..]
  return $ cars L.++ xs
  where
    names = ["apple", "cow", "cup", "dog", "horse", "pear", "tomato"]
