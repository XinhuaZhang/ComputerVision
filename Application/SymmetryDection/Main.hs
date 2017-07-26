import           Application.SymmetryDection.SymmetryDetection
import           Control.Monad                                 as M
import           CV.Filter.PinwheelWavelet
import           CV.IO.ImageIO
import           CV.Utility.Draw
import           CV.Utility.FFT
import           CV.Utility.RepaArrayUtility
import           Data.Array.Repa                               as R
import           Data.Complex
import           Data.List                                     as L
import           Data.Vector.Storable                          as VS
import           Data.Vector.Unboxed                           as VU
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import           System.Environment
import           Text.Printf
import CV.Utility.Parallel

main = do
  (imagePath:_) <- getArgs
  let point = (126, 104) -- (16,16) -- (100, 54)
      idx = (round $ fst point - 1) * n + (round $ snd point)
      alpha = L.head . pinwheelWaveletRadialScale $ filterParams
      r =
        round $
        (L.last . pinwheelWaveletRadius $ filterParams) / alpha +
        3 * (pinwheelWaveletGaussianScale filterParams) *
        (L.last . pinwheelWaveletScale $ filterParams) /
        alpha
      n = 128 -- 32 --128
      filterParams =
        PinwheelWaveletParams
        { pinwheelWaveletRows = n
        , pinwheelWaveletCols = n
        , pinwheelWaveletGaussianScale = 0.25
        , pinwheelWaveletScale = L.map (\x -> sqrt 2 ** x) [0 .. 3]
        , pinwheelWaveletRadialScale = L.map (\x -> (1 / sqrt 2) ** x) [0 .. 3]
        , pinwheelWaveletRadialFreqs = 3 / 4 * pi
        , pinwheelWaveletAngularFreqs = [-15 .. 15]
        , pinwheelWaveletRadius = [1 .. 3]
        }
      path = "fftw.dat"
      fftwWisdom = FFTWWisdomPath path
  print r
  fftwInit <- initializefftw FFTWWisdomNull
  img <-
    fmap
      (\x ->
          let (Z :. nf' :. _ :. _) = extent . imageContent $ x
              imageVec =
                VU.map (/ (fromIntegral nf')) .
                L.foldl1' (VU.zipWith (+)) .
                L.map
                  (\i ->
                      toUnboxed .
                      computeS . R.map (:+ 0) . R.slice (imageContent x) $
                      (Z :. i :. All :. All)) $
                [0 .. nf' - 1]
          in VU.convert imageVec) $
    readImageRepa imagePath True
  generateWisdom fftwInit path n n img
  fftw <- initializefftw fftwWisdom
  image <- readImageRepa imagePath True
  filters <- makeFilterConvolution fftw filterParams Normal :: IO PinwheelWaveletConvolution
  let (Z :. nf :. _ :. _) = extent . imageContent $ image
      imageVecs =
        L.map
          (\i ->
              VU.convert .
              toUnboxed . computeS . R.map (:+ 0) . R.slice (imageContent image) $
              (Z :. i :. All :. All))
          [0 .. nf - 1]
  filteredImages <- applyPinwheelWaveletConvolution fftw filters imageVecs
  plotImageRepa "circle.png" (draw image Yellow [Circle r 1 point])
  -- plotImageRepa
  --   "crop.png"
  --   (fmap
  --      (computeS .
  --       crop [round (snd point) - r, round (fst point) - r, 0] [2 * r, 2 * r, 3])
  --      image)
  let numPoints = 50
      normalizedFilteredImages =
        L.map
          (L.map
             (L.map
                (L.map
                   (parMap
                      rdeepseq
                      (VS.map
                         (\x ->
                             if (magnitude x) < 10 ** (-1)
                               then 0 :+ 0
                               else x))))))
          filteredImages
      rotation =
        rotationSymmetry (pinwheelWaveletAngularFreqs filterParams) numPoints normalizedFilteredImages
      rotationAvg =
        L.map (VS.map (/ fromIntegral numPoints) . L.foldl1' (VS.zipWith (+))) $
        rotation
      -- reflection =
      --   reflectionSymmetry
      --     (pinwheelWaveletAngularFreqs filterParams)
      --     numPoints
      --     normalizedFilteredImages
      -- reflectionAvg =
      --   L.map (VS.map (/ fromIntegral numPoints) . L.foldl1' (VS.zipWith (+))) $
      --   reflection
      reflection =
        L.map
          (parMap rdeepseq reflectionSymmetryDegree . L.transpose . L.map VS.toList) .
        reflectionSymmetry (pinwheelWaveletAngularFreqs filterParams) numPoints $
        normalizedFilteredImages
  M.zipWithM
    (\xs s ->
        let ys = L.head xs
        in printf "Scale: %.3f  reflection symmetry: %.5f\n" s ys)
    reflection
    (pinwheelWaveletRadialScale filterParams)
  -- M.zipWithM
  --   (\xs s ->
  --       let ys = L.map (VS.! idx) xs
  --       in printf "Scale: %.3f  rotation symmetry: %.5f\n" s (rotationSymmetryDegree ys))
  --   rotation
  --   (pinwheelWaveletRadialScale filterParams)
  -- M.zipWithM
  --   (\xs s ->
  --       let ys = L.map (VS.! idx) xs
  --       in printf "Scale: %.3f  reflection symmetry: %.5f\n" s (reflectionSymmetryDegree ys))
  --   reflection
  --   (pinwheelWaveletRadialScale filterParams)
  -- M.sequence $
  --   L.zipWith3
  --     (\xs avg rScale -> do
  --        let ys = L.map (VS.! idx) xs
  --            z = avg VS.! idx
  --        toFile def ("rotation_" L.++ show rScale L.++ ".png") $
  --          plot
  --            (line
  --               ""
  --               [ L.zip
  --                   (L.map
  --                      (\m ->
  --                          2 * pi / (fromIntegral numPoints) * fromIntegral m -
  --                          pi)
  --                      [0 .. numPoints - 1] :: [Double])
  --                   ys
  --               , L.map
  --                   (\m ->
  --                       ( 2 * pi / (fromIntegral numPoints) * fromIntegral m - pi
  --                       , z))
  --                   [0 .. numPoints - 1] :: [(Double, Double)]
  --               ]))
  --     rotation
  --     rotationAvg
  --     (pinwheelWaveletRadialScale filterParams)
  -- M.sequence $
  --   L.zipWith3
  --     (\xs avg rScale -> do
  --        let ys = L.map (VS.! idx) xs
  --            z = avg VS.! idx
  --        toFile def ("reflection_" L.++ show rScale L.++ ".png") $
  --          plot
  --            (line
  --               ""
  --               [ L.zip
  --                   (L.map
  --                      (\m -> pi / (fromIntegral numPoints) * fromIntegral m)
  --                      [0 .. numPoints - 1] :: [Double])
  --                   ys
  --               , L.map
  --                   (\m -> (pi / (fromIntegral numPoints) * fromIntegral m, z))
  --                   [0 .. numPoints - 1] :: [(Double, Double)]
  --               ]))
  --     reflection
  --     reflectionAvg
  --     (pinwheelWaveletRadialScale filterParams)
