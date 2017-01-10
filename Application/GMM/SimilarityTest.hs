{-# LANGUAGE BangPatterns #-}
import           Application.MultiDimensionalGMM.FisherKernel
import           Application.MultiDimensionalGMM.GMM
import           Application.MultiDimensionalGMM.MixtureModel
import           Control.Monad                  as M
import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           CV.Array.LabeledArray
import           CV.Feature.PolarSeparable
import           CV.Filter.PolarSeparableFilter
import           CV.Utility.Parallel
import           Data.Array.Repa                as R
import           Data.Conduit
import           Data.Conduit.Binary            as CB
import           Data.Conduit.List              as CL
import           Data.List                      as L
import           Data.Set                       as S
import           Data.Vector.Unboxed            as VU
import           Prelude                        as P
import           System.Environment

avgPoolConduit :: Conduit (Int, [VU.Vector Double]) (ResourceT IO) (Int,VU.Vector Double)
avgPoolConduit =
  awaitForever
    (\(label, xs) ->
        let !vec = VU.fromList $ L.map (\x -> VU.sum x / (fromIntegral $ VU.length x)) xs
            !norm = sqrt . VU.sum . VU.map (^2) $ vec
        in yield (label, VU.map (/norm) vec))

main = do
  (inputFile:gmmFile:_) <- getArgs
  gmm <- readGMM gmmFile :: IO [GMM]
  let parallelParams =
        ParallelParams
        { numThread = 4
        , batchSize = 4
        }
      filterParamsSet1 =
        PolarSeparableFilterParamsSet
        { getSizeSet = (0, 0)
        , getDownsampleFactorSet = 1
        , getScaleSet = S.fromDistinctAscList [8]
        , getRadialFreqSet = S.fromDistinctAscList [0 .. (4 - 1)]
        , getAngularFreqSet = S.fromDistinctAscList [0 .. (4 - 1)]
        , getNameSet = Pinwheels
        }
      filterParamsSet2 =
        PolarSeparableFilterParamsSet
        { getSizeSet = (0, 0)
        , getDownsampleFactorSet = 2
        , getScaleSet = S.fromDistinctAscList [8]
        , getRadialFreqSet = S.fromDistinctAscList [0 .. (4 - 1)]
        , getAngularFreqSet = S.fromDistinctAscList [0 .. (4 - 1)]
        , getNameSet = Pinwheels
        }
      filterParamsList = [filterParamsSet1]
      numTake = 4
      numDrop = 0
  labeledArray <-
    runResourceT $
    sourceFile inputFile $$ readLabeledImagebinaryConduit =$= CL.take numTake
  let imgs = L.map (\(LabeledArray _ arr) -> arr) . L.drop numDrop $ labeledArray
  xs <-
    runResourceT $
    sourceFile inputFile $$ readLabeledImagebinaryConduit =$=
    multiLayerMagnitudeVariedSizedConduit parallelParams filterParamsList 1 =$=
    (fisherVectorConduit1 parallelParams gmm) =$=
    CL.take numTake
  M.zipWithM_
    (\img i -> plotImage ("Images/" L.++ show i L.++ ".png") img)
    imgs
    [1 ..]
  let ys = L.drop numDrop xs
      zs = L.map (\(_, x) -> VU.toList x) $ ys
      lables = L.map fst ys
      as =
        [ L.sum $ L.zipWith (*) a b
        | a <- zs
        , b <- zs ]
  print lables
  M.mapM_ print $ L.transpose zs
  M.mapM_ print $ splitList (numTake - numDrop) as
  M.mapM_ (print . sumAllS) imgs
  M.zipWithM_ (\img i -> plotImage (show i L.++ ".png") img) imgs [1 ..]


splitList :: Int -> [a] -> [[a]]
splitList n xs 
  | L.null as = [] 
  | otherwise = as : splitList n bs
  where (as,bs) = L.splitAt n xs
