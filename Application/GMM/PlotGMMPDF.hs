module Main where

import           Application.GMM.Gaussian
import           Application.GMM.GMM
import           Application.GMM.MixtureModel
import           Data.Binary
import           Data.Vector                            as V
import           Data.Vector.Unboxed                    as VU
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import           Prelude                                as P
import           System.Environment

gaussian' :: Gaussian -> Int -> Double -> Double
gaussian' (Gaussian numDims' mu' sigma') ind x =
  exp (-(x - m) ^ 2 / 2 / s) / (s * sqrt (2 * pi))
  where
    s = sigma' VU.! ind
    m = mu' VU.! ind

getProb :: V.Vector (Model Gaussian) -> Int -> Double -> Double
getProb modelVec ind x =
  V.foldl' (\s (Model (w, gm)) -> s + w * gaussian' gm ind x) 0 modelVec

main = do
  (filePath:_) <- getArgs
  (MixtureModel n modelVec) <- decodeFile filePath :: IO GMM
  V.mapM_
    (\i ->
        toFile def (show i P.++ ".png") $
        do layout_title .= "PDF"
           plot
             (line
                ""
                [ [ (x, getProb modelVec i x)
                  | x <- [-10,-9.9 .. 10] ]
                ])) $
    V.generate n id
