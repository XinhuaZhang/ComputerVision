import           Application.Animal.Path
import           Control.Monad                as M
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           CV.IO.ImageIO
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility
import           Data.Array                   as Arr
import           Data.Array.Repa              as R
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Vector.Unboxed          as VU
import           System.Directory
import           System.Environment
import           System.FilePath


{-# INLINE centerCrop #-}

centerCrop :: R.Array U DIM3 Double -> R.Array D DIM3 Double
centerCrop arr =
  crop [minCol, minRow, 0] [maxCol - minCol + 1, maxRow - minRow + 1, nf] arr
  where
    (Z :. nf :. rows :. cols) = extent arr
    xs =
      L.map
        (findBound .
         L.map fst .
         L.filter (\(_, e) -> e > 0) .
         assocs . listArray ((0, 0), (rows - 1, cols - 1)) . VU.toList) .
      arrayToUnboxed $
      arr
    ((minRow, minCol), (maxRow, maxCol)) =
      L.foldl1'
        (\((minRow1, minCol1), (maxRow1, maxCol1)) ((minRow2, minCol2), (maxRow2, maxCol2)) ->
            ( (min minRow1 minRow2, min minCol1 minCol2)
            , (max maxRow1 maxRow2, max maxCol1 maxCol2)))
        xs

{-# INLINE findBound #-}

findBound :: (Ord a) => [(a,a)] -> ((a,a),(a,a))
findBound xs = ((L.minimum ys, L.minimum zs), (L.maximum ys, L.maximum zs))
  where
    (ys, zs) = L.unzip xs


convertSink :: ParallelParams
            -> [ImageTransformation]
            -> String
            -> Sink FilePath (ResourceT IO) ()
convertSink parallelParams transformationList sizeStr = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do ys <- liftIO $ M.mapM (\x -> readImageRepa x False) xs
        let zs =
              parMapChunk
                parallelParams
                rseq
                (\(Image depth arr') ->
                    let result =
                          L.head . padTransformImage 0 transformationList . centerCrop $
                          arr'
                    in deepSeqArray result (Image depth result))
                ys
        liftIO $
          M.zipWithM
            (\z x ->
                let dir = takeDirectory x
                    name = takeBaseName x
                in plotImageRepa (dir </> sizeStr </> name <.> "png") z)
            zs
            xs
        convertSink parallelParams transformationList sizeStr)


main = do
  (folderPath:sizeStr:_) <- getArgs
  let imageSize = read sizeStr :: Int
      parallelParams =
        ParallelParams
        { numThread = 7
        , batchSize = 140
        }
      transformationParams =
        ImageTransformationParams
        { imageTransformationParamsRows = imageSize
        , imageTransformationParamsCols = imageSize
        , rotationAngleParams = 0
        , scaleFactorRange = (1, 1)
        , contrastFactorARange = (1, 1)
        , contrastFactorBRange = (0, 0)
        }
  transformationList <- generateImageTransformation transformationParams
  xs <- listDirectory folderPath
  paths <-
    L.concat <$>
    M.mapM
      (\x -> do
         let path = folderPath </> x
         removePathForcibly (path </> sizeStr)
         createDirectory (path </> sizeStr)
         L.map (path </>) . L.filter (L.isSuffixOf ".jpg") <$> listDirectory path)
      xs
  runResourceT $
    sourceList paths $$ convertSink parallelParams transformationList sizeStr
