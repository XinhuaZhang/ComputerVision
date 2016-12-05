module Main where

import           Application.GMM.ArgsParser         as Parser
import           Application.GMM.FisherKernel
import           Application.GMM.GMM
import           Application.GMM.MixtureModel
import           Classifier.LibLinear
import           Control.Arrow
import           Control.Monad
import qualified Control.Monad.Parallel             as MP
import           Control.Parallel
import           CV.Array.LabeledArray
import           CV.Feature.PolarSeparableRepa
import           CV.Filter
import           CV.Filter.PolarSeparableFilter
import           CV.Filter.PolarSeparableFilterRepa
import           CV.IO.ImageIO
import           CV.Utility.Parallel                as Parallel
import           Data.Array.Repa                    as R
import           Data.Binary
import           Data.Complex                       as C
import           Data.Conduit
import           Data.Conduit.List                  as CL
import           Data.List                          as L
import           Data.Maybe                         as Maybe
import           Data.Set                           as S
import           Data.Time.LocalTime
import           Data.Vector                        as V
import           Data.Vector.Unboxed                as VU
import           Foreign.Ptr
import           Prelude                            as P
import           System.Environment


sliceConduit :: ParallelParams
             -> Conduit (LabeledArray DIM3 Double) IO (Int, [VU.Vector Double])
sliceConduit parallelParams =
  do xs <- CL.take (Parallel.batchSize parallelParams)
     unless (P.null xs)
            (do let ys =
                      parMapChunk
                        parallelParams
                        rdeepseq
                        (\(LabeledArray label arr) ->
                           let (Z :. nf :. ny :. nx) = extent arr
                           in (label
                              ,P.map (\i ->
                                        toUnboxed . computeS $
                                        R.slice arr (Z :. i :. All :. All)) $
                               [0 .. nf - 1]))
                        xs
                sourceList ys
                sliceConduit parallelParams)

main = do
  args <- getArgs
  if P.null args
    then error "run with --help to see options."
    else return ()
  params <- parseArgs args
  gmm <- readGMM (gmmFile params) :: IO [GMM]
  let parallelParams =
        ParallelParams
        { Parallel.numThread = Parser.numThread params
        , Parallel.batchSize = Parser.batchSize params
        }
      filterParams =
        PolarSeparableFilterParams
        { getRadius = 128
        , getScale = S.fromDistinctAscList (scale params)
        , getRadialFreq = S.fromDistinctAscList [0 .. (freq params - 1)]
        , getAngularFreq = S.fromDistinctAscList [0 .. (freq params - 1)]
        , getName = Pinwheels
        }
      filters =
        makeFilter filterParams :: PolarSeparableFilter (R.Array U DIM3 (C.Complex Double))
  print params
  readLabeledImagebinarySource (inputFile params) $$
    magnitudeLabeledArrayConduit'
      parallelParams
      filters
      (downsampleFactor params) =$=
  -- CL.map
  --   (\(LabeledArray label arr) ->
  --       let (Z :. nf :. ny :. nx) = extent arr
  --           r = (fromIntegral $ nx ^ 2 + ny ^ 2) / 4
  --           centerX = fromIntegral nx / 2
  --           centerY = fromIntegral ny / 2
  --           vec =
  --             P.map
  --               (\(a, b) ->
  --                   if (fromIntegral b - centerX) ^ 2 + (fromIntegral a - centerY) ^ 2 < r
  --                     then Just . toUnboxed . computeS $
  --                          R.slice arr (Z :. All :. a :. b)
  --                     else Nothing) $
  --             [ (i, j)
  --             | i <- [0 .. ny - 1]
  --             , j <- [0 .. nx - 1] ]
  --       in (label, V.fromList . Maybe.catMaybes $ vec))
    sliceConduit parallelParams =$=
    (fisherVectorConduit parallelParams gmm) =$=
    CL.map (fromIntegral *** (getFeature . VU.toList)) =$=
    predict (modelName params) ((modelName params) P.++ ".out")
