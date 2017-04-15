{-# LANGUAGE FlexibleContexts #-}
import           Application.FacialExpression.Conduit
import           Control.Monad                        as M
import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           CV.Array.LabeledArray
import           CV.IO.ImageIO
import           CV.Utility.Parallel
import           CV.V4Filter
import           Data.Array.Repa                      as R
import           Data.Conduit
import           Data.Conduit.List                    as CL
import           Data.List                            as L
import           Data.Vector.Unboxed                  as VU
import           System.Environment

main = do
  (imageListPath:labelListPath:isColorStr:sizeStr:degStr:trainStr:_) <- getArgs
  let parallelParams = ParallelParams {numThread = 4, batchSize = 120}
      isColor = read isColorStr :: Bool
      colorStr =
        if isColor
          then "Color"
          else "Gray"
  labels <-
    fmap (L.map (\x -> read x :: Int) . L.lines) . readFile $ labelListPath
  runResourceT $
    imagePathSource imageListPath $$ readImageConduit isColor =$=
    revertConduit parallelParams =$=
    mergeSource (sourceList labels) =$=
    CL.map (\(l, arr) -> LabeledArray l arr) =$=
    padResizeRotateLabeledArrayConduit
      parallelParams
      (read sizeStr :: Int)
      (read degStr :: Double) =$=
    -- padResizeImageConduit parallelParams (read sizeStr :: Int) 0 =$=
    writeLabeledImageBinarySink
      (trainStr L.++ "_" L.++ sizeStr L.++ "_" L.++ degStr L.++ "_" L.++
       colorStr L.++
       ".dat")
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
