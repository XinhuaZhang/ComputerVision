import           Application.ShiftableTest.FastWaveletTransform
import           Application.ShiftableTest.LaplacianPyramid
import           Application.ShiftableTest.SteerablePyramid
import           CV.IO.ImageIO
import           Data.Array.Repa                                as R
import           Data.Image
import           Data.List                                      as L
import           System.Environment

{-# INLINE l2Norm #-}

l2Norm :: GrayImage -> Double
l2Norm img =
  (sqrt . L.sum . L.map (^ (2 :: Int)) . pixelList $ img) / fromIntegral (r * c)
  where
    r = rows img
    c = cols img

similarity :: [GrayImage] -> [GrayImage] -> Double
similarity xs ys
  | L.length xs == L.length ys =
    (sqrt (L.sum $ L.zipWith (\x y -> (l2Norm x - l2Norm y) ^ (2 :: Int)) xs ys) /
     (fromIntegral . L.length $ xs))
  | otherwise = error "similarity: inputs don't have the same length."

main = do
  (filePath:shiftedfilePath:spNumStr:lpNumStr:fwtNumStr:_) <- getArgs
  repaImg <- readImageRepa filePath False
  shiftedRepaImg <- readImageRepa shiftedfilePath False
  let (Z :. _ :. rows :. cols) = extent . imageContent $ repaImg
      img =
        makeImage
          rows
          cols
          (\i j -> (imageContent repaImg) R.! (Z :. (0 :: Int) :. i :. j)) :: GrayImage
      shiftedImg =
        makeImage
          rows
          cols
          (\i j -> (imageContent shiftedRepaImg) R.! (Z :. (0 :: Int) :. i :. j)) :: GrayImage
      bs = [b31, b32, b33]
      spResults =
        L.concat $ steerablePyramid (read spNumStr :: Int) h0 l0 l1 bs img
      spShiftedResults =
        L.concat $ steerablePyramid (read spNumStr :: Int) h0 l0 l1 bs shiftedImg
      lpResults = laplacianPyramid (read lpNumStr :: Int) img
      lpShiftedResults = laplacianPyramid (read lpNumStr :: Int) shiftedImg
      fwtResults =
        L.concat $
        fastWaveletTransform
          (read fwtNumStr :: Int)
          daubechies4H0
          daubechies4H1
          img
      fwtShiftedResults =
        L.concat $
        fastWaveletTransform
          (read fwtNumStr :: Int)
          daubechies4H0
          daubechies4H1
          shiftedImg
  print $ similarity spResults spShiftedResults
  print $ similarity lpResults lpShiftedResults
  print $ similarity fwtResults fwtShiftedResults
