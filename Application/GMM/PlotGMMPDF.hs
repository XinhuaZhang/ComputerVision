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

getProb :: V.Vector (Model Gaussian) -> Double -> Double
getProb modelVec x =
  V.foldl' (\s (Model (w, gm)) -> s + w * gaussian gm x) 0 modelVec

main = do
  (filePath:_) <- getArgs
  models <- decodeFile filePath :: IO [GMM]
  let step = 0.01
  V.imapM_
    (\i (MixtureModel _ modelVec) ->
        toFile def (show i P.++ ".png") $
        do layout_title .= "PDF"
           plot
             (line
                ""
                [ [ (x, getProb modelVec x * step)
                  | x <- [-5,(-5 + step) .. 5] ]
                ])) $
    V.fromList models
