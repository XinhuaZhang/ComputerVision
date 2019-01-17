import           Application.ECCV2018.ArgsParser          as AP
import           Application.ECCV2018.Conduit
import           Application.ECCV2018.Convolution.Conduit
import           Classifier.LibLinear
import           Control.Monad                            as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.PolarSeparableFilter
import           CV.Utility.Parallel                      as Par
import           Data.Array.Repa
import           Data.Binary
import           Data.Conduit
import           Data.Conduit.Binary                      as CB
import           Data.Conduit.List                        as CL
import           Data.List                                as L
import           Data.Vector.Unboxed                      as VU
import           System.Environment
import           Text.Printf

{-# INLINE match #-}

match
  :: ParallelParams
  -> [(Double, VU.Vector Double)]
  -> [(Double, VU.Vector Double)]
  -> Double
match parallelParams original xs =
  let ys =
        parMapChunk
          parallelParams
          rdeepseq
          (\(labelX, x) ->
             let zs =
                   L.map
                     (\(labelO, o) -> (labelO, VU.sum $ VU.zipWith (*) x o))
                     original
                 prediction =
                   fst . L.maximumBy (\(_, a) (_, b) -> compare a b) $ zs
             in if prediction == labelX
                  then 1 :: Double
                  else 0)
          xs
  in (L.sum ys) / (fromIntegral . L.length $ xs)

main = do
  args <- getArgs
  params <- parseArgs args
  print params
  kmeansModel <- decodeFile (kmeansFile params)
  let parallelParams =
        ParallelParams
        { Par.numThread = AP.numThread params
        , Par.batchSize = AP.batchSize params
        }
      (originalDat:rsDat:rscnDat:[]) = splitSpace . inputFile $ params
  (filterParamsList:invariantScatteringFilterParamsList:_) <-
    fmap (\x -> read x :: [[PolarSeparableFilterParams]]) . readFile $
    (paramsFileName params)
  (_plan, filters) <-
    makePolarSeparableFilterConvolutionList getEmptyPlan filterParamsList
  (plan, invariantScatteringFilters) <-
    makePolarSeparableFilterConvolutionList
      _plan
      invariantScatteringFilterParamsList
  featureOriginal <-
    runResourceT $
    CB.sourceFile originalDat $$ readLabeledImagebinaryConduit =$=
    (if variedSizeImageFlag params
       then polarSeparableFilterConvolutionConduitVariedSize
              parallelParams
              plan
              filters
              invariantScatteringFilters
              (numScatteringLayer params)
       else polarSeparableFilterConvolutionConduit
              parallelParams
              plan
              filters
              invariantScatteringFilters
              (numScatteringLayer params)) =$=
    (if (extractObjectFlag params)
       then mergeSource
              (CB.sourceFile originalDat =$= readLabeledImagebinaryConduit) =$=
            objectFeatureExtractionConduit parallelParams (stride params)
       else invariantFeatureExtractionConduit parallelParams (stride params)) =$=
    kmeansConduit parallelParams kmeansModel =$=
    CL.consume
  featureRS <-
    runResourceT $
    CB.sourceFile rsDat $$ readLabeledImagebinaryConduit =$=
    (if variedSizeImageFlag params
       then polarSeparableFilterConvolutionConduitVariedSize
              parallelParams
              plan
              filters
              invariantScatteringFilters
              (numScatteringLayer params)
       else polarSeparableFilterConvolutionConduit
              parallelParams
              plan
              filters
              invariantScatteringFilters
              (numScatteringLayer params)) =$=
    (if (extractObjectFlag params)
       then mergeSource (CB.sourceFile rsDat =$= readLabeledImagebinaryConduit) =$=
            objectFeatureExtractionConduit parallelParams (stride params)
       else invariantFeatureExtractionConduit parallelParams (stride params)) =$=
    kmeansConduit parallelParams kmeansModel =$=
    CL.consume
  featureRSCN <-
    runResourceT $
    CB.sourceFile rscnDat $$ readLabeledImagebinaryConduit =$=
    (if variedSizeImageFlag params
       then polarSeparableFilterConvolutionConduitVariedSize
              parallelParams
              plan
              filters
              invariantScatteringFilters
              (numScatteringLayer params)
       else polarSeparableFilterConvolutionConduit
              parallelParams
              plan
              filters
              invariantScatteringFilters
              (numScatteringLayer params)) =$=
    (if (extractObjectFlag params)
       then mergeSource (CB.sourceFile rscnDat =$= readLabeledImagebinaryConduit) =$=
            objectFeatureExtractionConduit parallelParams (stride params)
       else invariantFeatureExtractionConduit parallelParams (stride params)) =$=
    kmeansConduit parallelParams kmeansModel =$=
    CL.consume
  printf "%.2f%%\n" $ (match parallelParams featureOriginal featureRS) * 100
  printf "%.2f%%\n" $ (match parallelParams featureOriginal featureRSCN) * 100
