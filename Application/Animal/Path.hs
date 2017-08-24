module Application.Animal.Path where

import           Control.Monad    as M
import           Data.List        as L
import           System.Directory
import           System.FilePath
import           System.Random

{-# INLINE shuffleList #-}

shuffleList :: [a] -> IO [a]
shuffleList xs = do
  ys <- M.replicateM (L.length xs) randomIO :: IO [Int]
  return . fst . L.unzip . L.sortOn snd $ L.zip xs ys

generateTrainTestPath :: FilePath -> Int -> IO ()
generateTrainTestPath folderPath imageSize = do
  xs <- listDirectory folderPath
  ys <-
    M.mapM
      (\x -> do
         let path = folderPath </> x </> show imageSize
         imageList <-
           L.map (path </>) . L.filter (L.isSuffixOf ".png") <$> listDirectory path
         shuffleList imageList)
      xs
  let (trainLabels, train) =
        L.unzip . L.concat $
        L.zipWith
          (\zs i -> L.map (\z -> (i, z)) . L.take 50 $ zs)
          ys
          [1 :: Double ..]
      (testLabels, test) =
        L.unzip . L.concat $
        L.zipWith
          (\zs i -> L.map (\z -> (i, z)) . L.drop 50 $ zs)
          ys
          [1 :: Double ..]
  writeFile (show imageSize L.++ "_trainImageList.txt") . L.unlines $ train
  writeFile (show imageSize L.++ "_trainLabelList.txt") . L.unlines . L.map show $
    trainLabels
  writeFile (show imageSize L.++ "_testImageList.txt") . L.unlines $ test
  writeFile (show imageSize L.++ "_testLabelList.txt") . L.unlines . L.map show $
    testLabels
