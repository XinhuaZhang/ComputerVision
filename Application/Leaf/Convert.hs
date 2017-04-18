{-# LANGUAGE FlexibleContexts #-}
import           Control.Monad                        as M
import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           CV.Array.LabeledArray
import           CV.IO.ImageIO
import           CV.Utility.Parallel
import           Data.Array.Repa                      as R
import           Data.Conduit
import           Data.Conduit.List                    as CL
import           Data.List                            as L
import           System.Environment
import           Text.Printf

main = do
  (imageListPath:labelListPath:isColorStr:trainStr:sizeStr:degStr:scaleFactorStr:aRangeStr:bRangeStr:_) <-
    getArgs
  let parallelParams =
        ParallelParams
        { numThread = 4
        , batchSize = 120
        }
      isColor = read isColorStr :: Bool
      colorStr =
        if isColor
          then "Color"
          else "Gray"
      (r, c) = read sizeStr :: (Int, Int)
      transformParams =
        ImageTransformationParams
        { imageTransformationParamsRows = r
        , imageTransformationParamsCols = c
        , rotationAngleParams = read degStr :: Double
        , scaleFactorRange = read scaleFactorStr :: (Double, Double)
        , contrastFactorARange = read aRangeStr :: (Double, Double)
        , contrastFactorBRange = read bRangeStr :: (Double, Double)
        }
  labels <- fmap (L.map (\x -> read x :: Int) . L.lines) . readFile $ labelListPath
  runResourceT $
    imagePathSource imageListPath $$ readImageConduit isColor =$=
    revertConduit parallelParams =$=
    mergeSource (sourceList labels) =$=
    CL.map (\(l, arr) -> LabeledArray l arr) =$=
    padTransformImageConduit parallelParams 0 transformParams =$=
    writeLabeledImageBinarySink
      (printf
         "%s_%s_%s_%s_%s_%s_%s.dat"
         trainStr
         colorStr
         sizeStr
         degStr
         scaleFactorStr
         aRangeStr
         bRangeStr)
      (L.length labels)

revertConduit
  :: (R.Source s Double)
  => ParallelParams
  -> Conduit (R.Array s DIM3 Double) (ResourceT IO) (R.Array U DIM3 Double)
revertConduit parallelParams = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rseq
                (\arr ->
                    let arr' = computeS . R.map (\x -> 255 - x) $ arr
                    in deepSeqArray arr' arr') $
              xs
        sourceList ys
        revertConduit parallelParams)
