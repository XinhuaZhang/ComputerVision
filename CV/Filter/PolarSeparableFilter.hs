{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module CV.Filter.PolarSeparableFilter where


import           CV.Filter.GaussianFilter
import           CV.FilterExpansion
import           CV.Utility.Coordinates
import           CV.Utility.RepaArrayUtility as RU
import           Data.Array.CArray           as CA
import           Data.Array.Repa             as R
import           Data.Complex                as C
import           Data.List                   as L
import           Data.Set                    as Set
import           Data.Vector.Unboxed         as VU
import           Foreign.Storable
import           Math.FFT
import           Prelude                     as P

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


data PolarSeparableFilterParamsGrid = PolarSeparableFilterParamsGrid
  { getPolarSeparableFilterGridRows         :: !Int
  , getPolarSeparableFilterGridCols         :: !Int
  , getPolarSeparableFilterRows             :: !Int
  , getPolarSeparableFilterCols             :: !Int
  , getPolarSeparableFilterDownsampleFactor :: !Int
  , getPolarSeparableFilterScale            :: ![Double]
  , getPolarSeparableFilterRadialFreq       :: ![Int]
  , getPolarSeparableFilterAngularFreq      :: ![Int]
  , getPolarSeparableFilterName             :: !PolarSeparableFilterName
  } deriving (Show)

type PolarSeparableFilterExpansion = PolarSeparableFilter PolarSeparableFilterParamsGrid [[VU.Vector (Complex Double)]]

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
     angleFunctionRad (P.fromIntegral x) (P.fromIntegral y))

{-# INLINE radialFunc #-}

radialFunc :: Double -> Int -> (Int -> Int -> Complex Double)
radialFunc scale freq x y =
  ejx
    ((sqrt . P.fromIntegral $ x ^ (2 :: Int) + y ^ (2 :: Int)) * fromIntegral freq *
     (2 * pi) /
     (4 * scale))
-- ejx
--   ((1 - exp (-1 * P.fromIntegral freq / 8)) *
--    (sqrt . P.fromIntegral $ x ^ (2 :: Int) + y ^ (2 :: Int)) *
--    pi)

{-# INLINE fans #-}

fans :: Double -> Int -> Int -> (Int -> Int -> Complex Double)
fans scale _rf af x y
  | scale == 0 = angularFunc af x y
  | otherwise = angularFunc af x y * real2Complex (gaussian2D scale x y)

{-# INLINE bullseye #-}

bullseye :: Double -> Int -> Int -> (Int -> Int -> Complex Double)
bullseye scale rf _af x y
  | scale == 0 = radialFunc scale rf x y
  | otherwise = radialFunc scale rf x y * real2Complex (gaussian2D scale x y)

{-# INLINE pinwheels #-}

pinwheels :: Double -> Int -> Int -> (Int -> Int -> Complex Double)
pinwheels scale rf af x y
  | scale == 0 = angularFunc af x y * radialFunc scale  rf x y
  | otherwise =
    real2Complex (gaussian2D' af rf scale x y) * angularFunc af x y * radialFunc scale rf x y

{-# INLINE getFilterFunc #-}

getFilterFunc
  :: PolarSeparableFilterParams
  -> (Double -> Int -> Int -> (Int -> Int -> Complex Double))
getFilterFunc PolarSeparableFilterParams {getName = Fans} = fans
getFilterFunc PolarSeparableFilterParams {getName = Bullseye} = bullseye
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


makeFilter :: PolarSeparableFilterParams -> PolarSeparableFilter PolarSeparableFilterParams (R.Array U DIM2 (Complex Double))
makeFilter params@(PolarSeparableFilterParams (ny, nx) downSampleFactor scale rf af _name) =
  PolarSeparableFilter params .
  computeS .
  twoDCArray2RArray . dftN [0, 1] . listArray ((0, 0), (ny' - 1, nx' - 1)) $
  makeFilterList ny' nx' (getFilterFunc params scale rf af)
  where
    ny' = div ny downSampleFactor
    nx' = div nx downSampleFactor

makeFilterSet
  :: PolarSeparableFilterParamsSet
  -> PolarSeparableFilter PolarSeparableFilterParamsSet (R.Array U DIM3 (Complex Double))
makeFilterSet params@(PolarSeparableFilterParamsSet (ny, nx) downSampleFactor scaleSet rfSet afSet _name) =
  PolarSeparableFilter params filterArr
  where
    !paramsList = generateParamsSet scaleSet rfSet afSet
    !filterEleList =
      L.map
        (\(scale, rf, af) ->
            makeFilterList ny' nx' (getFilterSetFunc params scale rf af))
        paramsList
    !ny' = div ny downSampleFactor
    !nx' = div nx downSampleFactor
    !nf' = L.length filterEleList
    !cArr = listArray ((0, 0, 0), (nf' - 1, ny' - 1, nx' - 1)) . L.concat $ filterEleList
    !dftCArr = dftN [1, 2] cArr
    !filterArr = computeS $ threeDCArray2RArray dftCArr

applyFilterFixedSize
  :: (Source s (Complex Double))
  => PolarSeparableFilter PolarSeparableFilterParams (R.Array U DIM2 (Complex Double))
  -> R.Array s DIM3 (Complex Double)
  -> R.Array D DIM3 (Complex Double) 
applyFilterFixedSize (PolarSeparableFilter params filter') =
  filterFunc (getDownsampleFactor params) filter'

applyFilterSetFixedSize
  :: (Source s (Complex Double))
  => PolarSeparableFilter PolarSeparableFilterParamsSet (R.Array U DIM3 (Complex Double))
  -> R.Array s DIM3 (Complex Double)
  -> R.Array D DIM3 (Complex Double) 
applyFilterSetFixedSize (PolarSeparableFilter params filter') =
  filterSetFunc (getDownsampleFactorSet params) filter'

applyFilterVariedSize
  :: (Source s (Complex Double))
  => PolarSeparableFilterParams
  -> R.Array s DIM3 (Complex Double)
  -> R.Array D DIM3 (Complex Double) 
applyFilterVariedSize (PolarSeparableFilterParams _ downsampleFactor scale rf af name) inputArr =
  filterFunc downsampleFactor filter' inputArr
  where
    (Z :. _ :. ny :. nx) = extent inputArr
    (PolarSeparableFilter _ !filter') =
      CV.Filter.PolarSeparableFilter.makeFilter
        (PolarSeparableFilterParams (ny, nx) downsampleFactor scale rf af name)  


applyFilterSetVariedSize
  :: (Source s (Complex Double))
  => PolarSeparableFilterParamsSet
  -> R.Array s DIM3 (Complex Double)
  -> R.Array D DIM3 (Complex Double) 
applyFilterSetVariedSize (PolarSeparableFilterParamsSet _ downsampleFactor scaleSet rfSet afSet name) inputArr =
  filterSetFunc downsampleFactor filter' inputArr
  where
    (Z :. _ :. ny :. nx) = extent inputArr
    (PolarSeparableFilter _ !filter') =
      makeFilterSet
        (PolarSeparableFilterParamsSet
           (ny, nx)
           downsampleFactor
           scaleSet
           rfSet
           afSet
           name)  

{-# INLINE filterFunc #-}

filterFunc
  :: (Source s (Complex Double))
  => Int
  -> R.Array U DIM2 (Complex Double)
  -> R.Array s DIM3 (Complex Double)
  -> R.Array D DIM3 (Complex Double)
filterFunc downsampleFactor filterArr inputArr =
  threeDCArray2RArray . idftN [1, 2] . threeDRArray2CArray $ multArr
  where
    !cArr =
      if downsampleFactor == 1
        then threeDRArray2CArray inputArr
        else threeDRArray2CArray $
             RU.downsample [downsampleFactor, downsampleFactor, 1] inputArr
    !dftCArr = dftN [1, 2] cArr
    !rArr = threeDCArray2RArray dftCArr
    !multArr =
      computeUnboxedS $
      traverse2
        rArr
        filterArr
        const
        (\f1 f2 idx@(Z :. _k :. j :. i) -> f1 idx * f2 (Z :. j :. i))


{-# INLINE filterSetFunc #-}

filterSetFunc
  :: (Source s (Complex Double))
  => Int
  -> R.Array U DIM3 (Complex Double)
  -> R.Array s DIM3 (Complex Double)
  -> R.Array D DIM3 (Complex Double)
filterSetFunc downsampleFactor filterArr inputArr =
  threeDCArray2RArray . idftN [1, 2] . threeDRArray2CArray $ multArr
  where
    !cArr =
      if downsampleFactor == 1
        then threeDRArray2CArray inputArr
        else threeDRArray2CArray $
             RU.downsample [downsampleFactor, downsampleFactor, 1] inputArr
    !dftCArr = dftN [1, 2] cArr
    !rArr = threeDCArray2RArray dftCArr
    !(Z :. nfFilter :. ny' :. nx') = extent filterArr
    !(Z :. nfInput :. _ :. _) = extent inputArr
    !newNf = nfFilter * nfInput
    !multArr =
      computeUnboxedS $
      fromFunction
        (Z :. newNf :. ny' :. nx')
        (\(Z :. k :. j :. i) ->
            let !kInput = mod k nfInput
                !kFilter = div k nfInput
            in rArr R.! (Z :. kInput :. j :. i) *
               filterArr R.! (Z :. kFilter :. j :. i))


{-# INLINE twoDCArray2RArray #-}

twoDCArray2RArray
  :: (Num a, Storable a)
  => CArray (Int, Int) a -> R.Array D DIM2 a
twoDCArray2RArray cArr =
  fromFunction
    (Z :. (ny' + 1) :. (nx' + 1))
    (\(Z :. j :. i) -> cArr CA.! (j, i))
  where
    ((_, _), (ny', nx')) = bounds cArr

{-# INLINE threeDRArray2CArray #-}

threeDRArray2CArray
  :: (Num a, Storable a, Source s a)
  => R.Array s DIM3 a -> CArray (Int, Int, Int) a
threeDRArray2CArray rArr =
  listArray ((0, 0, 0), (nf - 1, ny - 1, nx - 1)) . R.toList $ rArr
  where
    (Z :. nf :. ny :. nx) = extent rArr

{-# INLINE threeDCArray2RArray #-}

threeDCArray2RArray
  :: (Num a, Storable a)
  => CArray (Int, Int, Int) a -> R.Array D DIM3 a
threeDCArray2RArray cArr =
  fromFunction
    (Z :. (nf' + 1) :. (ny' + 1) :. (nx' + 1))
    (\(Z :. k :. j :. i) -> cArr CA.! (k, j, i))
  where
    ((_, _, _), (nf', ny', nx')) = bounds cArr

{-# INLINE makeFilterList #-}

makeFilterList :: Int -> Int -> (Int -> Int -> a) -> [a]
makeFilterList ny nx f =
  [ let !x =
          if r < (ny `div` 2)
            then r
            else r - ny
        !y =
          if c < (nx `div` 2)
            then c
            else c - nx
    in f x y
  | r <- [0 .. ny - 1]
  , c <- [0 .. nx - 1] ]

-- V4 Filter

instance FilterExpansion PolarSeparableFilterExpansion where
  type FilterParameter PolarSeparableFilterExpansion = PolarSeparableFilterParamsGrid
  {-# INLINE makeFilter #-}
  makeFilter (PolarSeparableFilter params@(PolarSeparableFilterParamsGrid gRows gCols rows cols downSampleFactor scaleSet rfSet afSet filterName) _) =
    PolarSeparableFilter params .
    L.map
      (\(centerR, centerC) ->
          [ VU.fromListN
             (newCols * newRows)
             [ getFilterByName
                filterName
                scale
                rf
                af
                (c - centerC)
                (r - centerR)
             | r <- [0 .. newRows - 1]
             , c <- [0 .. newCols - 1] ]
          | scale <- scaleSet
          , rf <- rfSet
          , af <- afSet ] L.++
          L.map
            ((\(scale, rf, af) ->
                 VU.fromListN
                   (newCols * newRows)
                   [ getFilterByName
                      filterName
                      scale
                      rf
                      af
                      (c - centerC)
                      (r - centerR)
                   | r <- [0 .. newRows - 1]
                   , c <- [0 .. newCols - 1] ]) .
             (\(scale, rf, af) ->
                 if rf /= 0 && af /= 0
                   then (scale, rf, -af)
                   else (scale, -rf, -af)))
            [ (scale, rf, af)
            | scale <- scaleSet
            , rf <- rfSet
            , af <- afSet ]) $
    grid2D (newRows, newCols) (gRows, gCols)
    where
      newCols = div cols downSampleFactor
      newRows = div rows downSampleFactor
  getFilterSize (PolarSeparableFilter params _) =
    getFilterNumList params * getPolarSeparableFilterGridRows params *
    getPolarSeparableFilterGridCols params
  getFilterParameter (PolarSeparableFilter params _) = params
  {-# INLINE getFilterVectors #-}
  getFilterVectors (PolarSeparableFilter _ vecs) = vecs
  {-# INLINE changeSizeParameter #-}
  changeSizeParameter rows cols (PolarSeparableFilter (PolarSeparableFilterParamsGrid gRows gCols _ _ downsampleFactor scaleSet rfSet afSet name) vecs) =
    PolarSeparableFilter
      (PolarSeparableFilterParamsGrid
         gRows
         gCols
         rows
         cols
         downsampleFactor
         scaleSet
         rfSet
         afSet
         name)
      vecs

{-# INLINE getFilterByName #-}

getFilterByName
  :: PolarSeparableFilterName
  -> (Double -> Int -> Int -> (Int -> Int -> Complex Double))
getFilterByName Fans = fans
getFilterByName Bullseye = bullseye
getFilterByName Pinwheels = pinwheels

{-# INLINE getFilterNumList #-}

getFilterNumList :: PolarSeparableFilterParamsGrid -> Int
getFilterNumList (PolarSeparableFilterParamsGrid _ _ _ _ _ scale rs as _) =
  (P.product . P.map L.length $ [rs, as]) * L.length scale
