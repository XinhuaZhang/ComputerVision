{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module CV.Filter.PolarSeparableFilter where


import           CV.Filter.GaussianFilter
import           CV.FilterConvolution
import           CV.FilterExpansion
import           CV.Utility.Coordinates
import           CV.Utility.FFT
import           Data.Array.Repa          as R
import           Data.Complex             as C
import           Data.List                as L
import           Data.Set                 as Set
import           Data.Vector.Storable     as VS
import           Data.Vector.Unboxed      as VU
import           Foreign.Storable
import           Prelude                  as P

data PolarSeparableFilterName
  = Fans
  | Bullseye
  | Pinwheels
  deriving (Show, Read)

data PolarSeparableFilterParamsSet = PolarSeparableFilterParamsSet
  { getSizeSet             :: !(Int, Int) -- (ny,nx) or (rows,cols)
  , getDownsampleFactorSet :: !Int
  , getScaleSet            :: !(Set Double)
  , getRadialFreqSet       :: !(Set Int)
  , getAngularFreqSet      :: !(Set Int)
  , getNameSet             :: !PolarSeparableFilterName
  } deriving (Show)

data PolarSeparableFilter a b =
  PolarSeparableFilter !a
                       b
  deriving (Show)

data PolarSeparableFilterParams = PolarSeparableFilterParams
  { getSize             :: !(Int, Int)
  , getDownsampleFactor :: !Int
  , getScale            :: !Double
  , getRadialFreq       :: !Int
  , getAngularFreq      :: !Int
  , getName             :: !PolarSeparableFilterName
  } deriving (Show)


data PolarSeparableFilterParamsAxis = PolarSeparableFilterParamsAxis
  { getPolarSeparableFilterAxisRows        :: !Int
  , getPolarSeparableFilterAxisCols        :: !Int
  , getPolarSeparableFilterAxisPolarFactor :: !Double
  , getPolarSeparableFilterAxisScale       :: ![Double]
  , getPolarSeparableFilterAxisFreq        :: ![Int]
  , getPolarSeparableFilterAxisAngle       :: ![Double]
  } deriving (Show)


data PolarSeparableFilterParamsAxisConvolution = PolarSeparableFilterParamsAxisConvolution
  { getPolarSeparableFilterAxisConvolutionRows        :: !Int
  , getPolarSeparableFilterAxisConvolutionCols        :: !Int
  , getPolarSeparableFilterAxisConvolutionPolarFactor :: !Double
  , getPolarSeparableFilterAxisConvolutionScale       :: ![Double]
  , getPolarSeparableFilterAxisConvolutionFreq        :: ![Int]
  , getPolarSeparableFilterAxisConvolutionAngle       :: ![Double]
  } deriving (Show)


data PolarSeparableFilterParamsAxisInteger = PolarSeparableFilterParamsAxisInteger
  { getPolarSeparableFilterAxisIntegerRows              :: !Int
  , getPolarSeparableFilterAxisIntegerCols              :: !Int
  , getPolarSeparableFilterAxisIntegerScale             :: ![Double]
  , getPolarSeparableFilterAxisIntegerFreq              :: ![Int]
  , getPolarSeparableFilterAxisIntegerRadialMultiplier  :: ![Int]
  , getPolarSeparableFilterAxisIntegerAngularMultiplier :: ![Int]
  } deriving (Show)

data PolarSeparableFilterParamsGrid = PolarSeparableFilterParamsGrid
  { getPolarSeparableFilterGridRows        :: !Int
  , getPolarSeparableFilterGridCols        :: !Int
  , getPolarSeparableFilterGridScale       :: ![Double]
  , getPolarSeparableFilterGridRadialFreq  :: ![Int]
  , getPolarSeparableFilterGridAngularFreq :: ![Int]
  } deriving (Show)

data FourierMellinTransformParamsGrid = FourierMellinTransformParamsGrid
  { getFourierMellinTransformGridRows        :: !Int
  , getFourierMellinTransformGridCols        :: !Int
  , getFourierMellinTransformGridScale       :: ![Double]
  , getFourierMellinTransformGridRadialFreq  :: ![Double]
  , getFourierMellinTransformGridAngularFreq :: ![Int]
  } deriving (Show,Read)

data FourierMellinTransformParamsGridC = FourierMellinTransformParamsGridC
  { getFourierMellinTransformGridCRows        :: !Int
  , getFourierMellinTransformGridCCols        :: !Int
  , getFourierMellinTransformGridCScale       :: ![Double]
  , getFourierMellinTransformGridCRadialFreq  :: ![Int]
  , getFourierMellinTransformGridCAngularFreq :: ![Int]
  } deriving (Show)

type PolarSeparableFilterExpansionAxis = PolarSeparableFilter PolarSeparableFilterParamsAxis V4SeparableFilter
type PolarSeparableFilterConvolutionAxis = PolarSeparableFilter PolarSeparableFilterParamsAxisConvolution V4SeparableFilterConvolution
type PolarSeparableFilterExpansionAxisInteger = PolarSeparableFilter PolarSeparableFilterParamsAxisInteger V4SeparableFilter
type PolarSeparableFilterExpansionGrid = PolarSeparableFilter PolarSeparableFilterParamsGrid V4SeparableFilter
type FourierMellinTransformExpansionGrid = PolarSeparableFilter FourierMellinTransformParamsGrid V4SeparableFilter
type FourierMellinTransformExpansionGridC =PolarSeparableFilter  FourierMellinTransformParamsGridC V4SeparableFilter

-- this function is to make sure that the params sequence is correct
{-# INLINE generateParamsSet #-}
generateParamsSet :: Set Double -> Set Int -> Set Int -> [(Double,Int,Int)]
generateParamsSet scaleSet rfSet afSet =
  [ (scale, rf, af)
  | scale <- toAscList scaleSet
  , rf <- toAscList rfSet
  , af <- toAscList afSet ]

generatePSFParamsSet :: PolarSeparableFilterParamsSet
                     -> [PolarSeparableFilterParams]
generatePSFParamsSet (PolarSeparableFilterParamsSet (ny, nx) downsampleFactor scaleSet rfSet afSet name) =
  L.map
    (\(scale, rf, af) ->
        PolarSeparableFilterParams (ny, nx) downsampleFactor scale rf af name) $
  generateParamsSet scaleSet rfSet afSet



-- Input: [layer1, layer2 ...]
generateMultilayerPSFParamsSet :: [PolarSeparableFilterParamsSet]
                               -> [[PolarSeparableFilterParams]]
generateMultilayerPSFParamsSet =
  L.foldl'
    (\bss as ->
        [ a : bs
        | bs <- bss
        , a <- as ])
    [[]] .
  L.map generatePSFParamsSet . L.reverse


{-# INLINE ejx #-}

ejx
  :: (RealFloat a)
  => a -> Complex a
ejx x = exp (0 :+ x)

{-# INLINE real2Complex #-}

real2Complex
  :: (RealFloat a)
  => a -> Complex a
real2Complex x = x :+ 0

{-# INLINE angularFunc #-}

angularFunc :: Int -> (Int -> Int -> Complex Double)
angularFunc freq x y =
  ejx
    (P.fromIntegral freq *
     angleFunctionRad (P.fromIntegral x) (P.fromIntegral y)) -- / pi

{-# INLINE radialFunc #-}

radialFunc :: Double -> Int -> Int -> (Int -> Int -> Complex Double)
radialFunc scale af rFreq x y
  | r == 0 = 1
  | otherwise = exp (0 :+  fromIntegral rFreq * (log r) )
  -- ejx ((2 * pi) * fromIntegral rFreq * r / (3 * scale + r0)) -- /
  -- ((2 * scale + r0) :+ 0) *
  -- 2
  where
    r = sqrt . P.fromIntegral $ x ^ (2 :: Int) + y ^ (2 :: Int)
    r0 = 0 --((1 - exp (-0.01 * fromIntegral (abs af))) * 75 * scale) / pi


{-# INLINE fans #-}

fans :: Double -> Int -> Int -> (Int -> Int -> Complex Double)
fans scale _rf af x y
  | scale == 0 = angularFunc af x y
  | otherwise = angularFunc af x y * real2Complex (gaussian2D scale x y)

{-# INLINE bullseye #-}

bullseye :: Double -> Int -> Int -> (Int -> Int -> Complex Double)
bullseye scale rf af x y
  | scale == 0 = radialFunc scale af rf x y
  | otherwise = radialFunc scale af rf x y * real2Complex (gaussian2D scale x y)

{-# INLINE pinwheels #-}

pinwheels :: Double -> Int -> Int -> (Int -> Int -> Complex Double)
pinwheels scale rf af x y
  | scale == 0 = angularFunc af x y * radialFunc scale af rf x y
  | otherwise =
    -- real2Complex (gaussian2D' af rf scale x y) *
    angularFunc af x y * radialFunc scale af rf x y
    -- real2Complex (gaussian2D scale x y) * angularFunc af x y * radialFunc scale af rf x y

{-# INLINE pinwheelsC #-}

pinwheelsC :: Double -> Int -> Int -> (Int -> Int -> Complex Double)
pinwheelsC scale rf af x y
  | scale == 0 = conjugate (angularFunc af x y) * radialFunc scale af rf x y
  | otherwise =
    real2Complex (gaussian2D' af rf scale x y) * conjugate (angularFunc af x y) *
    radialFunc scale af rf x y

{-# INLINE pinwheelsAngle #-}

pinwheelsAngle :: Double
               -> Int
               -> Double
               -> Double
               -> (Int -> Int -> Complex Double)
pinwheelsAngle scale freq angle polarFactor x y
  | rho == 0 =  real2Complex (gaussian2D'' freq scale x y)
  | angle == pi / 2 =
    real2Complex (gaussian2D'' freq scale x y) *
    ejx (fromIntegral freq * polarFactor * log rho)
  | otherwise =
    real2Complex (gaussian2D'' freq scale x y) *
    ejx
      (fromIntegral freq *
       (theta + polarFactor * tan angle * log rho))
  where
    rho = sqrt . P.fromIntegral $ x ^ (2 :: Int) + y ^ (2 :: Int)
    theta = angleFunctionRad (P.fromIntegral x) (P.fromIntegral y)


{-# INLINE pinwheelsAngleC #-}

pinwheelsAngleC :: Double
                -> Int
                -> Double
                -> Double
                -> (Int -> Int -> Complex Double)
pinwheelsAngleC scale freq angle polarFactor x y
  | rho == 0 = real2Complex (gaussian2D'' freq scale x y)
  | angle == pi / 2 =
    real2Complex (gaussian2D'' freq scale x y) *
    ejx (fromIntegral freq * polarFactor * log rho)
  | otherwise =
    real2Complex (gaussian2D'' freq scale x y) *
    ejx
      (fromIntegral freq *
       (theta - polarFactor * tan angle * log rho))
  where
    rho = sqrt . P.fromIntegral $ x ^ (2 :: Int) + y ^ (2 :: Int)
    theta = angleFunctionRad (P.fromIntegral x) (P.fromIntegral y)


{-# INLINE pinwheelsAxis #-}

pinwheelsAxis :: Double -> Int -> Int -> Int -> Int -> Int -> Complex Double
pinwheelsAxis scale freq rm am x y
  | r == 0 = 1
  | otherwise =
    -- real2Complex (gaussian2D' (freq * am) (freq * rm) scale x y) *
    (ejx
       (P.fromIntegral (freq * am) *
        angleFunctionRad (P.fromIntegral x) (P.fromIntegral y)) *
    (ejx ( fromIntegral (freq * rm) * (log r))))
  where
    r = sqrt . P.fromIntegral $ x ^ (2 :: Int) + y ^ (2 :: Int)

{-# INLINE getFilterFunc #-}

getFilterFunc
  :: PolarSeparableFilterParams
  -> (Double -> Int -> Int -> (Int -> Int -> Complex Double))
getFilterFunc PolarSeparableFilterParams {getName = Fans}      = fans
getFilterFunc PolarSeparableFilterParams {getName = Bullseye}  = bullseye
getFilterFunc PolarSeparableFilterParams {getName = Pinwheels} = pinwheels

{-# INLINE getFilterSetFunc #-}

getFilterSetFunc
  :: PolarSeparableFilterParamsSet
  -> (Double -> Int -> Int -> (Int -> Int -> Complex Double))
getFilterSetFunc PolarSeparableFilterParamsSet {getNameSet = Fans} = fans
getFilterSetFunc PolarSeparableFilterParamsSet {getNameSet = Bullseye} = bullseye
getFilterSetFunc PolarSeparableFilterParamsSet {getNameSet = Pinwheels} = pinwheels

{-# INLINE getFilterNum #-}
getFilterNum :: PolarSeparableFilterParamsSet -> Int
getFilterNum (PolarSeparableFilterParamsSet _ _ scale rs as _) =
  (P.product . P.map Set.size $ [rs, as]) * Set.size scale


makeFilter
  :: FFTW
  -> PolarSeparableFilterParams
  -> IO (PolarSeparableFilter PolarSeparableFilterParams (R.Array U DIM2 (Complex Double)))
makeFilter fftw params@(PolarSeparableFilterParams (ny, nx) downSampleFactor scale rf af _name) =
  fmap (PolarSeparableFilter params . fromUnboxed (Z :. ny :. nx) . VS.convert) .
  dft2d fftw ny nx . VS.fromListN (ny * nx) $
  makeFilterList ny' nx' (getFilterFunc params scale rf af)
  where
    ny' = div ny downSampleFactor
    nx' = div nx downSampleFactor

-- makeFilterSet
--   :: FFTW
--   -> PolarSeparableFilterParamsSet
--   -> IO (PolarSeparableFilter PolarSeparableFilterParamsSet (R.Array U DIM3 (Complex Double)))
-- makeFilterSet fftw params@(PolarSeparableFilterParamsSet (ny, nx) downSampleFactor scaleSet rfSet afSet _name) =
--   PolarSeparableFilter params filterArr
--   where
--     !paramsList = generateParamsSet scaleSet rfSet afSet
--     !filterEleList =
--       L.map
--         (\(scale, rf, af) ->
--             makeFilterList ny' nx' (getFilterSetFunc params scale rf af))
--         paramsList
--     !ny' = div ny downSampleFactor
--     !nx' = div nx downSampleFactor
--     !nf' = L.length filterEleList
--     !cArr = listArray ((0, 0, 0), (nf' - 1, ny' - 1, nx' - 1)) . L.concat $ filterEleList
--     !dftCArr = dftN [1, 2] cArr
--     !filterArr = computeS $ threeDCArray2RArray dftCArr

-- applyFilterFixedSize
--   :: (Source s (Complex Double))
--   => PolarSeparableFilter PolarSeparableFilterParams (R.Array U DIM2 (Complex Double))
--   -> R.Array s DIM3 (Complex Double)
--   -> R.Array D DIM3 (Complex Double)
-- applyFilterFixedSize (PolarSeparableFilter params filter') =
--   filterFunc (getDownsampleFactor params) filter'

-- applyFilterSetFixedSize
--   :: (Source s (Complex Double))
--   => PolarSeparableFilter PolarSeparableFilterParamsSet (R.Array U DIM3 (Complex Double))
--   -> R.Array s DIM3 (Complex Double)
--   -> R.Array D DIM3 (Complex Double)
-- applyFilterSetFixedSize (PolarSeparableFilter params filter') =
--   filterSetFunc (getDownsampleFactorSet params) filter'

-- applyFilterVariedSize
--   :: (Source s (Complex Double))
--   => PolarSeparableFilterParams
--   -> R.Array s DIM3 (Complex Double)
--   -> R.Array D DIM3 (Complex Double)
-- applyFilterVariedSize (PolarSeparableFilterParams _ downsampleFactor scale rf af name) inputArr =
--   filterFunc downsampleFactor filter' inputArr
--   where
--     (Z :. _ :. ny :. nx) = extent inputArr
--     (PolarSeparableFilter _ !filter') =
--       CV.Filter.PolarSeparableFilter.makeFilter
--         (PolarSeparableFilterParams (ny, nx) downsampleFactor scale rf af name)


-- applyFilterSetVariedSize
--   :: (Source s (Complex Double))
--   => PolarSeparableFilterParamsSet
--   -> R.Array s DIM3 (Complex Double)
--   -> R.Array D DIM3 (Complex Double)
-- applyFilterSetVariedSize (PolarSeparableFilterParamsSet _ downsampleFactor scaleSet rfSet afSet name) inputArr =
--   filterSetFunc downsampleFactor filter' inputArr
--   where
--     (Z :. _ :. ny :. nx) = extent inputArr
--     (PolarSeparableFilter _ !filter') =
--       makeFilterSet
--         (PolarSeparableFilterParamsSet
--            (ny, nx)
--            downsampleFactor
--            scaleSet
--            rfSet
--            afSet
--            name)

-- {-# INLINE filterFunc #-}

-- filterFunc
--   :: (Source s (Complex Double))
--   => Int
--   -> R.Array U DIM2 (Complex Double)
--   -> R.Array s DIM3 (Complex Double)
--   -> R.Array D DIM3 (Complex Double)
-- filterFunc downsampleFactor filterArr inputArr =
--   threeDCArray2RArray . idftN [1, 2] . threeDRArray2CArray $ multArr
--   where
--     !cArr =
--       if downsampleFactor == 1
--         then threeDRArray2CArray inputArr
--         else threeDRArray2CArray $
--              RU.downsample [downsampleFactor, downsampleFactor, 1] inputArr
--     !dftCArr = dftN [1, 2] cArr
--     !rArr = threeDCArray2RArray dftCArr
--     !multArr =
--       computeUnboxedS $
--       traverse2
--         rArr
--         filterArr
--         const
--         (\f1 f2 idx@(Z :. _k :. j :. i) -> f1 idx * f2 (Z :. j :. i))


-- {-# INLINE filterSetFunc #-}

-- filterSetFunc
--   :: (Source s (Complex Double))
--   => Int
--   -> R.Array U DIM3 (Complex Double)
--   -> R.Array s DIM3 (Complex Double)
--   -> R.Array D DIM3 (Complex Double)
-- filterSetFunc downsampleFactor filterArr inputArr =
--   threeDCArray2RArray . idftN [1, 2] . threeDRArray2CArray $ multArr
--   where
--     !cArr =
--       if downsampleFactor == 1
--         then threeDRArray2CArray inputArr
--         else threeDRArray2CArray $
--              RU.downsample [downsampleFactor, downsampleFactor, 1] inputArr
--     !dftCArr = dftN [1, 2] cArr
--     !rArr = threeDCArray2RArray dftCArr
--     !(Z :. nfFilter :. ny' :. nx') = extent filterArr
--     !(Z :. nfInput :. _ :. _) = extent inputArr
--     !newNf = nfFilter * nfInput
--     !multArr =
--       computeUnboxedS $
--       fromFunction
--         (Z :. newNf :. ny' :. nx')
--         (\(Z :. k :. j :. i) ->
--             let !kInput = mod k nfInput
--                 !kFilter = div k nfInput
--             in rArr R.! (Z :. kInput :. j :. i) *
--                filterArr R.! (Z :. kFilter :. j :. i))

-- V4 Filter

instance FilterExpansion PolarSeparableFilterExpansionAxis where
  type FilterParameter PolarSeparableFilterExpansionAxis = PolarSeparableFilterParamsAxis
  type FilterType PolarSeparableFilterExpansionAxis = V4SeparableFilter
  {-# INLINE makeFilter #-}
  makeFilter (PolarSeparableFilter params@(PolarSeparableFilterParamsAxis rows cols pf scales freqs angles) _) (centerR, centerC) =
    PolarSeparableFilter params .
    V4PolarSeparableFilterAxis (L.map fromIntegral freqs) $
    [ [ VU.fromListN (cols * rows) $
       [ pinwheelsAngle scale freq angle pf (c - centerC) (r - centerR)
       | r <- [0 .. rows - 1]
       , c <- [0 .. cols - 1] ]
      | freq <- freqs ]
    | scale <- scales
    , angle <- radAngles2 ]
    where
      radAngles = L.map deg2Rad angles
      radAngles2 = radAngles L.++ L.map (+ (pi / 2)) radAngles
  getFilterSize (PolarSeparableFilter params _) = undefined -- getFilterNumList params * 2
  getFilterParameter (PolarSeparableFilter params _) = params
  {-# INLINE getFilterVectors #-}
  getFilterVectors (PolarSeparableFilter _ vecs) = vecs
  {-# INLINE changeSizeParameter #-}
  changeSizeParameter rows cols (PolarSeparableFilter (PolarSeparableFilterParamsAxis _ _ pf scaleSet freqs ams) vecs) =
    PolarSeparableFilter
      (PolarSeparableFilterParamsAxis rows cols pf scaleSet freqs ams)
      vecs

instance FilterConvolution PolarSeparableFilterConvolutionAxis where
  type FilterConvolutionParamsType PolarSeparableFilterConvolutionAxis = PolarSeparableFilterParamsAxisConvolution
  {-# INLINE makeConvolutionFilter #-}
  makeConvolutionFilter params@(PolarSeparableFilterParamsAxisConvolution rows cols pf scales freqs angles) =
    PolarSeparableFilter params .
    V4PolarSeparableFilterConvolutionAxis
      (rows, cols)
      (L.map fromIntegral freqs) $
    [ [ VS.fromListN (cols * rows) $
       makeFilterList rows cols (pinwheelsAngle scale freq angle pf)
      | freq <- freqs ]
    | scale <- scales
    , angle <- radAngles2 ]
    where
      radAngles = L.map deg2Rad angles
      radAngles2 = radAngles L.++ L.map (+ (pi / 2)) radAngles
  getConvolutionFilterSize (PolarSeparableFilter _params _) = undefined -- getFilterNumList params * 2
  {-# INLINE changeSizeParameterConvolution #-}
  changeSizeParameterConvolution rows cols (PolarSeparableFilter (PolarSeparableFilterParamsAxisConvolution _ _ pf scaleSet freqs ams) vecs) =
    PolarSeparableFilter
      (PolarSeparableFilterParamsAxisConvolution rows cols pf scaleSet freqs ams)
      vecs


instance FilterExpansion PolarSeparableFilterExpansionAxisInteger where
  type FilterParameter PolarSeparableFilterExpansionAxisInteger = PolarSeparableFilterParamsAxisInteger
  type FilterType PolarSeparableFilterExpansionAxisInteger = V4SeparableFilter
  {-# INLINE makeFilter #-}
  makeFilter (PolarSeparableFilter params@(PolarSeparableFilterParamsAxisInteger rows cols scales freqs rms ams) _) (centerR, centerC) =
    PolarSeparableFilter params .
    V4PolarSeparableFilterAxis (L.map fromIntegral freqs) $
    [ [ VU.fromListN
         (cols * rows)
         [ conjugate $ pinwheelsAxis scale freq rm am (c - centerC) (r - centerR)
         | r <- [0 .. rows - 1]
         , c <- [0 .. cols - 1] ]
      | freq <- freqs ]
    | scale <- scales
    , (rm, am) <- L.zip rms ams ]
  getFilterSize (PolarSeparableFilter params _) = undefined --  getFilterNumList params * 2
  getFilterParameter (PolarSeparableFilter params _) = params
  {-# INLINE getFilterVectors #-}
  getFilterVectors (PolarSeparableFilter _ vecs) = vecs
  {-# INLINE changeSizeParameter #-}
  changeSizeParameter rows cols (PolarSeparableFilter (PolarSeparableFilterParamsAxisInteger _ _ scaleSet freqs rms ams) vecs) =
    PolarSeparableFilter
      (PolarSeparableFilterParamsAxisInteger rows cols scaleSet freqs rms ams)
      vecs

{-# INLINE getFilterByName #-}

getFilterByName
  :: PolarSeparableFilterName
  -> (Double -> Int -> Int -> (Int -> Int -> Complex Double))
getFilterByName Fans      = fans
getFilterByName Bullseye  = bullseye
getFilterByName Pinwheels = pinwheels

-- {-# INLINE getFilterNumList #-}

-- getFilterNumList :: PolarSeparableFilterParamsAxis -> Int
-- getFilterNumList (PolarSeparableFilterParamsAxis _ _ scales freqs rms ) =
--   L.length scales * L.length freqs * (min (L.length rms) (L.length ams)) * 2

instance FilterExpansion PolarSeparableFilterExpansionGrid where
  type FilterParameter PolarSeparableFilterExpansionGrid = PolarSeparableFilterParamsGrid
  type FilterType PolarSeparableFilterExpansionGrid  = V4SeparableFilter
  {-# INLINE makeFilter #-}
  makeFilter (PolarSeparableFilter params@(PolarSeparableFilterParamsGrid rows cols scales rfs afs) _) (centerR, centerC) =
    PolarSeparableFilter params .
    V4PolarSeparableFilterGrid (L.map fromIntegral rfs, L.map fromIntegral afs) $
    [ [ [ VU.fromListN
      (cols * rows)
      [ pinwheels scale rf af (c - centerC) (r - centerR)
      | r <- [0 .. rows - 1]
      , c <- [0 .. cols - 1]
      ]
    | af <- afs
    ]
    | rf <- rfs
    ]
    | scale <- scales
    ]
  getFilterSize (PolarSeparableFilter params _) = undefined
  getFilterParameter (PolarSeparableFilter params _) = params
  {-# INLINE getFilterVectors #-}
  getFilterVectors (PolarSeparableFilter _ vecs) = vecs
  {-# INLINE changeSizeParameter #-}
  changeSizeParameter rows cols (PolarSeparableFilter (PolarSeparableFilterParamsGrid _ _ scaleSet rfSet afSet) vecs) =
    PolarSeparableFilter
      (PolarSeparableFilterParamsGrid rows cols scaleSet rfSet afSet)
      vecs




-- Fourier-Mellin Transform

{-# INLINE fourierMellinTransform #-}

fourierMellinTransform :: Double -> Double -> Int -> Int -> Int -> Complex Double
fourierMellinTransform scale rf af x y
  | x == 0 && y == 0 = 0
  | otherwise =
    -- ((fromIntegral x :+ fromIntegral y) ** (fromIntegral (-af) :+ 0)) *
    -- ((sqrt (fromIntegral x ^ (2 :: Int) + fromIntegral y ^ (2 :: Int))) **
    --  (((fromIntegral af - 1) :+ (-rf)))) /
    -- (2 * pi)
       -- (0 :+ disk scale x y) *
       (r ** ((-0.5) :+ (-rf))) *
       -- exp (0 :+ ((-rf) * log r)) *
       exp (0 :+ (fromIntegral (-af) * theta))
       where
         r = sqrt . P.fromIntegral $ x ^ (2 :: Int) + y ^ (2 :: Int)
         theta = angleFunctionRad (P.fromIntegral x) (P.fromIntegral y)



{-# INLINE fourierMellinTransformC #-}

fourierMellinTransformC :: Double -> Int -> Int -> Int -> Int -> Complex Double
fourierMellinTransformC scale rf af x y
  | r == 0 = 0
  | otherwise = -- (0 :+ disk scale x y) *
    (r ** ((-0.5) :+ (fromIntegral (rf)))) *
    exp (0 :+ (fromIntegral (af) * theta))
  where
    r = sqrt . P.fromIntegral $ x ^ (2 :: Int) + y ^ (2 :: Int)
    theta = angleFunctionRad (P.fromIntegral x) (P.fromIntegral y)


instance FilterExpansion FourierMellinTransformExpansionGrid where
  type FilterParameter FourierMellinTransformExpansionGrid = FourierMellinTransformParamsGrid
  type FilterType FourierMellinTransformExpansionGrid  = V4SeparableFilter
  {-# INLINE makeFilter #-}
  makeFilter (PolarSeparableFilter params@(FourierMellinTransformParamsGrid rows cols scales rfs afs) _) (centerR, centerC) =
    PolarSeparableFilter params .
    FourierMellinTransform ( rfs, L.map fromIntegral afs) $
    [ [ [ VU.fromListN
           (cols * rows) $
          --  makeFilterList rows cols (fourierMellinTransform scale rf af)
           [ fourierMellinTransform scale rf af (c - centerC) (r - centerR)
           | r <- [0 .. rows - 1]
           , c <- [0 .. cols - 1] ]
        | af <- afs ]
      | rf <- rfs ]
    | scale <- scales ]
  getFilterSize (PolarSeparableFilter params _) = undefined
  getFilterParameter (PolarSeparableFilter params _) = params
  {-# INLINE getFilterVectors #-}
  getFilterVectors (PolarSeparableFilter _ vecs) = vecs
  {-# INLINE changeSizeParameter #-}
  changeSizeParameter rows cols (PolarSeparableFilter (FourierMellinTransformParamsGrid _ _ scaleSet rfSet afSet) vecs) =
    PolarSeparableFilter
      (FourierMellinTransformParamsGrid rows cols scaleSet rfSet afSet)
      vecs


instance FilterExpansion FourierMellinTransformExpansionGridC where
  type FilterParameter FourierMellinTransformExpansionGridC = FourierMellinTransformParamsGridC
  type FilterType FourierMellinTransformExpansionGridC = V4SeparableFilter
  {-# INLINE makeFilter #-}
  makeFilter (PolarSeparableFilter params@(FourierMellinTransformParamsGridC rows cols scales rfs afs) _) (centerR, centerC) =
    PolarSeparableFilter params .
    FourierMellinTransform (L.map fromIntegral rfs, L.map fromIntegral afs) $
    [ [ [ VU.fromListN
           (cols * rows)
           [ fourierMellinTransformC scale rf af (c - centerC) (r - centerR)
           | r <- [0 .. rows - 1]
           , c <- [0 .. cols - 1] ]
        | af <- afs -- L.++ (L.map (\x -> -x) . L.tail $ afs)
          ]
      | rf <- rfs -- L.++ (L.map (\x -> -x) . L.tail $ rfs)
        ]
    | scale <- scales ]
  getFilterSize (PolarSeparableFilter params _) = undefined
  getFilterParameter (PolarSeparableFilter params _) = params
  {-# INLINE getFilterVectors #-}
  getFilterVectors (PolarSeparableFilter _ vecs) = vecs
  {-# INLINE changeSizeParameter #-}
  changeSizeParameter rows cols (PolarSeparableFilter (FourierMellinTransformParamsGridC _ _ scaleSet rfSet afSet) vecs) =
    PolarSeparableFilter
      (FourierMellinTransformParamsGridC rows cols scaleSet rfSet afSet)
      vecs
