{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveGeneric #-}
module Application.GMM.MixtureModel where

import           Application.GMM.Gaussian
import           Data.Binary
import           Data.Time
import           Data.Vector              as V
import           GHC.Generics
import           Prelude                  as P
import           System.Random


newtype Model a =
  Model (Double,a)
  deriving (Show,Generic)

instance (Binary a) =>
         Binary (Model a) where
  put (Model x) = put x
  get =
    do x <- get
       return $ Model x

data MixtureModel a =
  MixtureModel {numModel :: Int
               ,model    :: V.Vector (Model a)}
  deriving (Show,Generic)

instance (Binary a) =>
         Binary (MixtureModel a) where
  put (MixtureModel numModel' xs) =
    do put numModel'
       put $ V.toList xs
  get =
    do numModel' <- get
       xs <- get
       return (MixtureModel numModel'
                            (V.fromList xs))


randomRList :: (RandomGen g,Random a)
            => Int -> (a,a) -> g -> ([a],g)
randomRList len bound gen
  | len > 0 =
    (\(xs,g) -> (x : xs,g)) $
    randomRList (len - 1)
                bound
                newGen
  | otherwise = ([],gen)
  where (x,newGen) = randomR bound gen

initializeMixture
  :: Int -> V.Vector a -> IO (MixtureModel a)
initializeMixture numModel xs =
  do time <- getCurrentTime
     let gen =
           mkStdGen . P.fromIntegral . diffTimeToPicoseconds . utctDayTime $
           time
         (w',gen1) =
           randomRList numModel
                       (1,1000)
                       gen
         !ws' = P.sum $ w'
         w = V.fromList $ P.map (/ ws') w'
         models = V.zipWith (\a b -> Model (a,b)) w xs
     return (MixtureModel numModel models)
