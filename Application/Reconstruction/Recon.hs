module Application.Reconstruction.Recon where

import           Data.Array.Repa     as R
import           Data.Complex
import           Data.List           as L
import           Data.Vector.Unboxed as VU

computeRecon
  :: R.Array D DIM3 (Complex Double)
  -> [VU.Vector (Complex Double)]
  -> R.Array U DIM3 (Complex Double)
computeRecon img filters =
  fromUnboxed (Z :. nf' :. ny' :. nx') .
  VU.concat .
  L.map
    (\imgVec ->
        let coef = L.map (VU.sum . VU.zipWith (*) imgVec . VU.map conjugate) filters
        in L.foldl1' (VU.zipWith (+)) $
           L.zipWith (\filterVec c -> VU.map (* c) filterVec) filters coef) $
  imgList
  where
    (Z :. nf' :. ny' :. nx') = extent img
    imgList =
      L.map
        (\i ->
            toUnboxed . computeUnboxedS . R.slice img $ (Z :. i :. All :. All))
        [0 .. nf']
