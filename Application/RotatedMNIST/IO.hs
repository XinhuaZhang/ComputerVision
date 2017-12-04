module Application.RotatedMNIST.IO where

import           Control.Monad                as M
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           Data.Array.Repa              as R
import           Data.Char
import           Data.Conduit                 as C
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Text.Lazy               as TL
import           Data.Text.Lazy.IO            as TL
import           Data.Text.Lazy.Read          as TL
import           System.IO

{-# INLINE readDoubles #-}

readDoubles :: Text -> [Double]
readDoubles txt
  | TL.null xs = []
  | otherwise =
    case (double xs) of
      Left msg        -> error msg
      Right (x, txt1) -> x : readDoubles txt1
  where
    xs = TL.dropWhile (not . isDigit) $ txt

{-# INLINE readImageLabel #-}

readImageLabel :: Text -> LabeledArray DIM3 Double
readImageLabel txt =
  let xs = readDoubles txt
  in LabeledArray (round . L.last $ xs) .
     computeS .
     R.backpermute
       (Z :. (1 :: Int) :. (28 :: Int) :. (28 :: Int))
       (\(Z :. a :. b :. c) -> (Z :. a :. c :. b)) .
     fromListUnboxed (Z :. (1 :: Int) :. (28 :: Int) :. (28 :: Int)) . L.init $
     xs

imageLabelSource :: FilePath
                 -> C.Source (ResourceT IO) (LabeledArray DIM3 Double)
imageLabelSource filePath = do
  h <- liftIO $ openFile filePath ReadMode
  loop h
  where
    loop h = do
      flag <- liftIO $ hIsEOF h
      if flag
        then liftIO $ hClose h
        else do
          xs <- liftIO $ TL.hGetLine h
          yield . readImageLabel $ xs
          loop h
