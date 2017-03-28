import           Control.Monad            as M
import           CV.Filter.GaussianFilter
import           Data.Array
import           Data.Complex
import qualified Data.Image               as IM
import           Data.List                as L
import           CV.Utility.Coordinates

cartesianSeparable :: Double -> Double -> Int -> Int -> Complex Double
cartesianSeparable xFreq yFreq x y =
  exp (0 :+ xFreq * fromIntegral (x)) * exp (0 :+ yFreq * fromIntegral (y))

hyperbolicSeparable :: Double -> Double -> Double -> Int -> Int -> Complex Double
hyperbolicSeparable theta uFreq vFreq x y
  | x' == 0 || y' == 0 = 1
  | otherwise = exp (0 :+ uFreq * u) * exp (0 :+ vFreq * v)
  where
    c = cos theta
    s = sin theta
    x' = fromIntegral x * c - fromIntegral y * s
    y' = fromIntegral x * s + fromIntegral y * c
    u = log . sqrt . abs $ x' / y'
    v = sqrt . abs $ x' * y'
    

hyperbolicSeparable1 :: Double -> Double -> Double -> Int -> Int -> Complex Double
hyperbolicSeparable1 theta uFreq vFreq x y
  | r == 0 || t == 0 = 1
  | otherwise = exp (0 :+ uFreq * u) * exp (0 :+ vFreq * v)
  where
    r = (sqrt . fromIntegral $ x ^ (2 :: Int) + y ^ (2 :: Int)) / pi
    t = angleFunctionRad (fromIntegral x) (fromIntegral y)
    u = log . sqrt . abs $ t / r
    v = sqrt . abs $  r * t


main = do
  let xFreq = [0,0.05..0.5]
      yFreq = xFreq
      n = 128
      centerR = div n 2
      centerC = div n 2
      scale = 48
      cartesianSeparableFilters =
        [ [ (cartesianSeparable xf yf (x - centerC) (y - centerC)) *
           gaussian2D scale (x - centerC) (y - centerC)
          | x <- [0 .. n - 1]
          , y <- [0 .. n - 1] ]
        | xf <- xFreq
        , yf <- yFreq ]
      uFreq = [0]
      vFreq = 0 : (L.reverse $ L.map (\i -> 1 / (2 ^ i)) [0 .. 6])
      hTheta = L.map deg2Rad [0]
      hyperbolicSeparableFilters =
        L.concatMap
          (\theta ->
              [ [ (hyperbolicSeparable theta uf vf (x - centerC) (y - centerC)) *
                 gaussian2D scale (x - centerC) (y - centerC)
                | x <- [0 .. n - 1]
                , y <- [0 .. n - 1] ]
              | uf <- uFreq
              , vf <- vFreq ])
          hTheta
      plotFilter name =
        IM.writeImage name .
        (\arr -> IM.arrayToImage arr :: IM.ComplexImage) .
        listArray ((0, 0), (n - 1, n - 1))
  M.zipWithM_
    (\i img ->
        plotFilter ("CartesianSeparableFilter_" L.++ show i L.++ ".ppm") img)
    [0 ..]
    cartesianSeparableFilters
  -- M.zipWithM_
  --   (\i img ->
  --       plotFilter ("HyperbolicSeparableFilter_" L.++ show i L.++ ".ppm") img)
  --   [0 ..]
  --   hyperbolicSeparableFilters
