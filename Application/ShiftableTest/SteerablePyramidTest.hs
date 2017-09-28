import           Application.ShiftableTest.SteerablePyramid
import           Control.Monad                              as M
import           CV.IO.ImageIO
import           Data.Array.Repa                            as R
import           Data.Image
import           Data.List                                  as L
import           System.Environment
import           System.FilePath

main = do
  (filePath:numStr:_) <- getArgs
  repaImg <- readImageRepa filePath False
  let (Z :. _ :. rows :. cols) = extent . imageContent $ repaImg
      img =
        makeImage
          rows
          cols
          (\i j -> (imageContent repaImg) R.! (Z :. (0 :: Int) :. i :. j)) :: GrayImage
      bs = [b31, b32, b33]
      results = steerablePyramid (read numStr :: Int) h0 l0 l1 bs img
  plotSteerablePyramid (dropExtension filePath) results
-- writeImage (dropExtension filePath L.++ "_h0.pgm") . L.head . L.head $ results
-- writeImage (dropExtension filePath L.++ "_l0l1.pgm") . L.head . L.last $ results
-- M.zipWithM
--   (\i xs ->
--       M.zipWithM
--         (\j x ->
--             writeImage
--               (dropExtension filePath L.++ "_b" L.++ show i L.++ show j L.++
--                ".pgm")
--               x)
--         [1 ..]
--         xs)
--   [1 ..] .
--   L.init . L.tail $
--   results
