module CV.Utility.DFT
  ( module CV.Utility.DFT.Plan
  , importFFTWWisdom
  , exportFFTWWisdom
  ) where

import           Control.Monad       (unless)
import           CV.Utility.DFT.Plan
import           CV.Utility.FFT.Base (exportWisdomString, importWisdomString)


importFFTWWisdom :: FilePath -> IO ()
importFFTWWisdom path = do
  str <- readFile path
  flag <- importWisdomString str
  unless flag (error $ "initializefftw: importWisdomString (" ++ str ++ ")")
  
exportFFTWWisdom :: FilePath -> IO ()
exportFFTWWisdom path = do
  wisdom <- exportWisdomString
  writeFile path wisdom
