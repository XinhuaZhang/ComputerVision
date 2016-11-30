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
  exp (-(x - m) ^ 2 / 2 / s) / (sqrt (2 * pi * s))
  where
    s = sigma' VU.! ind
    m = mu' VU.! ind

getProb :: V.Vector (Model Gaussian) -> Int -> Double -> Double
getProb modelVec ind x =
  V.foldl' (\s (Model (w, gm)) -> s + w * gaussian' gm ind x) 0 modelVec

main = do
  (filePath:_) <- getArgs
  (MixtureModel _ modelVec) <- decodeFile filePath :: IO GMM
  let (Model (_,(Gaussian n _ _))) = V.head modelVec
  V.mapM_
    (\i ->
        toFile def (show i P.++ ".png") $
        do layout_title .= "Feature " P.++ show i
           plot
             (line
                ""
                [ [ (x, getProb modelVec i x)
                  | x <- [0,0.05 .. 20] ]
                ])) $
    V.generate n id
