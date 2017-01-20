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
  models <- readGMM filePath :: IO [GMM]
  V.imapM_
    (\i (MixtureModel _ modelVec) ->
        toFile def (show i P.++ ".png") $
        do let range = 2 * (getMaxMu modelVec)
               step = (log (getMaxMu modelVec)) / 20
           layout_title .= "PDF"
           plot
             (line
                ""
                [ [ (x, getProb modelVec x * step)
                  | x <- [-range,(-range + step) .. range] ]
                ])) $
    V.fromList models
    
getMaxMu :: V.Vector (Model Gaussian) -> Double
getMaxMu  = V.maximum . V.map (\(Model (_,Gaussian mu _)) -> mu)
