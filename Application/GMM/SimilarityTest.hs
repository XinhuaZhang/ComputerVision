{-# LANGUAGE BangPatterns #-}
import           Application.GMM.PCA
import           Application.MultiDimensionalGMM.FisherKernel
import           Application.MultiDimensionalGMM.GMM
import           Application.MultiDimensionalGMM.MixtureModel
import           Control.Monad                                as M
import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           CV.Array.LabeledArray
import           CV.Feature.PolarSeparable
import           CV.Filter.PolarSeparableFilter
import           CV.Utility.Parallel
import           Data.Array.Repa                              as R
import           Data.Conduit
import           Data.Conduit.Binary                          as CB
import           Data.Conduit.List                            as CL
import           Data.List                                    as L
import           Data.Set                                     as S
import           Data.Vector.Unboxed                          as VU
import           Prelude                                      as P
import           System.Environment

avgPoolConduit :: Conduit (Int, [VU.Vector Double]) (ResourceT IO) (Int,VU.Vector Double)
avgPoolConduit =
  awaitForever
    (\(label, xs) ->
        let !vec = VU.fromList $ L.map (\x -> VU.sum x / (fromIntegral $ VU.length x)) xs
            !norm = sqrt . VU.sum . VU.map (^2) $ vec
        in yield (label, VU.map (/norm) vec))

main =
  do (inputFile:gmmFile:pcaFile:_) <- getArgs
     gmm <- readGMM gmmFile :: IO [GMM]
     pcaMatrixes <- readMatrixes pcaFile
     let parallelParams =
           ParallelParams {numThread = 4
                          ,batchSize = 4}
         filterParamsSet1 =
           PolarSeparableFilterParamsSet {getSizeSet = (0,0)
                                         ,getDownsampleFactorSet = 1
                                         ,getScaleSet =
                                            S.fromDistinctAscList [4]
                                         ,getRadialFreqSet =
                                            S.fromDistinctAscList [0 .. (8 - 1)]
                                         ,getAngularFreqSet =
                                            S.fromDistinctAscList [0 .. (8 - 1)]
                                         ,getNameSet = Pinwheels}
         filterParamsSet2 =
           PolarSeparableFilterParamsSet {getSizeSet = (0,0)
                                         ,getDownsampleFactorSet = 2
                                         ,getScaleSet =
                                            S.fromDistinctAscList [8]
                                         ,getRadialFreqSet =
                                            S.fromDistinctAscList [0 .. (4 - 1)]
                                         ,getAngularFreqSet =
                                            S.fromDistinctAscList [0 .. (4 - 1)]
                                         ,getNameSet = Pinwheels}
         filterParamsList = [filterParamsSet1]
         numTake = 200
         numDrop = 192
     labeledArray <-
       readLabeledImagebinarySource inputFile $$
       (do CL.drop numDrop
           aa <- CL.take (numTake - numDrop)
           return aa)
     print . L.length $ labeledArray
     let imgs = L.map (\(LabeledArray _ arr) -> arr) labeledArray
     xs <-
       runResourceT $
       sourceList labeledArray $$
       multiLayerMagnitudeVariedSizedConduit parallelParams filterParamsList 1 =$=
       pcaLabelMultiLayerConduit parallelParams pcaMatrixes =$=
       (fisherVectorConduit1 parallelParams gmm) =$=
       consume
     M.zipWithM_ (\img i -> plotImage ("Images/" L.++ show i L.++ ".png") img)
                 imgs
                 [1 ..]
     let ys =  xs
         zs = L.map (VU.toList . snd) ys
         labels = L.map fst ys
         as = [L.sum $ L.zipWith (*) a b|a <- zs,b <- zs]
     print labels
     putStrLn "hehe"
     M.mapM_ print $ L.transpose zs
     putStrLn "hehe"
     M.mapM_ print $ splitList (numTake - numDrop) as-- M.mapM_ (print . sumAllS) imgs

splitList :: Int -> [a] -> [[a]]
splitList n xs
  | L.null as = []
  | otherwise = as : splitList n bs
  where (as,bs) = L.splitAt n xs
