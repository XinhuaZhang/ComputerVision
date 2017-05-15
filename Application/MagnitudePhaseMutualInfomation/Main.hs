import           Control.Monad                as M
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Statistics.Histogram
import           CV.V4Filter                  as V4
import           CV.V4FilterConvolution
import           Data.Array                   as Arr
import           Data.Array.Repa              as R
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.Binary          as CB
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Vector.Unboxed          as VU
import           System.Environment
import           System.Random

randomConduit :: Conduit (LabeledArray DIM3 Double) (ResourceT IO) (LabeledArray DIM3 Double)
randomConduit =
  awaitForever
    (\(LabeledArray l arr) -> do
       let (Z :. nf' :. ny' :. nx') = extent arr
       xs <- liftIO $ M.replicateM (ny' * nx') $ randomRIO (0, 255)
       yield . LabeledArray l . fromListUnboxed (Z :. (1 :: Int) :. ny' :. nx') $
         xs)

main = do
  (imageListPath:_) <- getArgs
  let (ny, nx) = (256, 256)
      filterParams =
        FourierMellinTransformParamsGrid
        { getFourierMellinTransformGridRows = ny
        , getFourierMellinTransformGridCols = nx
        , getFourierMellinTransformGridScale = [2 * pi]
        , getFourierMellinTransformGridRadialFreq = [4]
        , getFourierMellinTransformGridAngularFreq = [4]
        }
  filtersF <-
    fourierTransformFilter (ny, nx) $
    getFilterVectors
      (V4.makeFilter
         (PolarSeparableFilter filterParams Null :: FourierMellinTransformExpansionGrid)
         (div nx 2, div ny 2))
  xs <-
    runResourceT $
    CB.sourceFile imageListPath $$ readLabeledImagebinaryConduit =$= randomConduit =$=
    -- CL.map
    --   (\(LabeledArray l arr) ->
    --       let (Z :. nf' :. ny' :. nx') = extent arr
    --           vec =
    --             L.foldl1' (VU.zipWith (+)) .
    --             L.map
    --               (\i ->
    --                   R.toUnboxed . R.computeS . R.slice arr $
    --                   (Z :. i :. All :. All)) $
    --             [0 .. nf' - 1]
    --       in LabeledArray l . fromUnboxed (Z :. (1 :: Int) :. ny' :. nx') $ vec) =$=
    applyV4SeparableFilterConvolutionLabeledArrayConduit [filtersF] =$=
    CL.take 1
  let y =
        (\(FourierMellinTransformFilteredImageConvolution _ _ vecs) ->
            VU.filter (\x -> fst x /= 0) .
            VU.map (\x -> (magnitude x, phase x)) . L.head . L.head . L.head $
            vecs) .
        L.head . snd . L.head $
        xs
      m = VU.foldl' (\a (b, _) -> max a b) 0 y
      (mag, pha) =
        VU.unzip . VU.map (\(a, b) -> (a / m, (b + pi) / (2 * pi))) $ y
      -- VU.unzip . VU.map (\(a, b) -> (a / m, (sin b + 1) / 2)) $ y
      histParams = KdHistParams 100 (1 / 100) False 1
      features =
        L.map (VU.fromListN 2) . L.transpose . L.map VU.toList $ [mag, pha]
      hist = build histParams features
  print . mutualInformation $ hist
  print . entropy . (\x -> fmap fromIntegral $ computeMarginalHistogram x [0]) $
    hist
  print . entropy . (\x -> fmap fromIntegral $ computeMarginalHistogram x [1]) $
    hist
