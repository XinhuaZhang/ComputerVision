{-# LANGUAGE BangPatterns #-}
module Main where

import           Application.GMM.Gaussian
import           Application.GMM.GMM
import           Application.GMM.MixtureModel
import           CV.Filter.PolarSeparableFilter
import           CV.Image                       as IM
import           CV.Utility.Parallel            as Parallel
import           Data.Array
import           Data.Binary
import           Data.Complex                   as C
import           Data.List                      as L
import           Data.Set                       as S
import           Data.Vector                    as V
import           Data.Vector.Unboxed            as VU
import           Prelude                        as P
import           System.Directory
import Control.Monad as M


main =
  do gmm <- decodeFile "8x8_8_128_GMM.dat.bak" :: IO GMM
     let parallelParams =
           ParallelParams {Parallel.numThread = 4
                          ,Parallel.batchSize = 8}
         muVec = V.map (\(Model (w,gm)) -> mu gm) . model $ gmm
         rs = S.fromDistinctAscList [0 .. 7]
         as = S.fromDistinctAscList [0 .. 7]
         ss = S.fromDistinctAscList [8]
         filterParams =
           PolarSeparableFilterParams {getRadius = 128
                                      ,getScale = ss
                                      ,getRadialFreq = rs
                                      ,getAngularFreq = as
                                      ,getName = Pinwheels}
         !filterList =
           withStrategy (parList rseq) $
           [IM.makeImage
              (2 * getRadius filterParams)
              (2 * getRadius filterParams)
              (\x y ->
                 getFilterFunc filterParams
                               s
                               rf
                               af
                               (x - getRadius filterParams)
                               (y - getRadius filterParams)) :: ComplexImage
           |rf <- S.toList rs
           ,af <- S.toList as
           ,s <- S.toList ss]
         !weight =
           parMapChunkVector
             parallelParams
             rseq
             (\mu    
                ->
                IM.magnitude .
                arrayToImage .
                listArray ((0,0)
                          ,((2 * getRadius filterParams) - 1
                           ,(2 * getRadius filterParams) - 1)) .
                L.foldl1' (L.zipWith (+)) .
                P.zipWith (\f m ->
                             P.map (\(a :+ b) -> (a * m) :+ (b * m)) .
                             pixelList $
                             f)
                          filterList .
                VU.toList $
                mu :: GrayImage)
             muVec
     removePathForcibly "./Weight"
     createDirectoryIfMissing True "./Weight"
     createDirectoryIfMissing True "./Weight/Pinwheel"
     V.imapM_ (\i w ->
                 writeImage ("./Weight/" P.++ show i P.++ ".pgm")
                            w)
              weight
     M.zipWithM_
       (\i w ->
          writeImage ("./Weight/Pinwheel/" P.++ show i P.++ ".ppm")
                     w)
       [0 ..]
       filterList
