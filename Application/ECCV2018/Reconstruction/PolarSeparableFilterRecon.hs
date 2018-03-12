import           Application.ECCV2018.Reconstruction.Recon
import           Application.ECCV2018.Utility
import           Control.Monad                             as M
import           Control.Monad.Trans.Resource
import           CV.Filter.PolarSeparableFilter
import           CV.IO.ImageIO
import           CV.Utility.FFT
import           Data.Array                                as Arr
import           Data.Array.Repa                           as R
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.List                         as CL
import qualified Data.Image                                as IM
import           Data.List                                 as L
import           Data.Vector.Storable                      as VS
import           Data.Vector.Unboxed                       as VU
import           System.Environment
import           System.FilePath

{-# INLINE concatStr #-}

concatStr :: [String] -> String
concatStr (x:[]) = x
concatStr (x:xs) = x L.++ "_" L.++ (concatStr xs)

main = do
  (imagePath:alphaStr:thresholdStr:lrStr:writeStepStr:filterNameList) <- getArgs
  print filterNameList
  img <- readImageRepa imagePath False
  let (Z :. _ :. rows :. cols) = extent . imageContent $ img
      filterParamsList =
        L.map (filterParamsFunc rows cols (read alphaStr) . read) filterNameList
      imgVec =
        normalizeImage (-1, 1) .
        VU.convert . VU.map (:+ 0) . toUnboxed . imageContent $
        img
  M.mapM_ print filterParamsList
  (plan_, filters) <-
    makePolarSeparableFilterConvolutionList getEmptyPlan filterParamsList
  (plan, filtersPI) <-
    makePolarSeparableFilterConvolutionPIList plan_ filterParamsList
  magnitudeReconConvolution
    plan
    rows
    cols
    (1 * (0.1 ** (read lrStr :: Double)))
    (read thresholdStr :: Double)
    imgVec
    (L.concatMap getFilterListConvolutionFunc filters)
    (L.concatMap getFilterListConvolutionFunc filtersPI)
    NULL
    (read writeStepStr :: Int)
    ("MagnitudeRecon" </> concatStr filterNameList)
