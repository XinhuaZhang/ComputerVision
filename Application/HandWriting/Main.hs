import           Application.HandWriting.Conduit
import           Application.HandWriting.IO
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Conduit.Binary             as CB
import           Data.Conduit.List               as CL
import           Data.IntMap.Strict
import           Data.List                       as L
import           Data.Vector.Unboxed             as VU
import           Data.Word
import           System.Environment


main =
  do (path:labelMapPath:_) <- getArgs
     labelMapStr <- readFile labelMapPath
     -- ((a,b),(c,d)) <-
     --   runResourceT $
     --   hwdbSource path $$ offlineCharacterConduit =$=
     --   CL.fold (\((ws,wc),(hs,hc)) (OfflineCharacter _ w h _) ->
     --              ((ws + fromIntegral w,wc + 1),(hs + fromIntegral h,hc + 1)))
     --           ((0 :: Int,0 :: Int),(0 :: Int,0 :: Int))
     -- print (fromIntegral a / fromIntegral b,fromIntegral c / fromIntegral d)
     -- (a,b) <-
     --   runResourceT $
     --   hwdbSource path $$ offlineCharacterConduit
     -- CL.fold (\(maxW,maxH) (OfflineCharacter _ w h _) -> (max maxW w, max maxH h))
     --         (0,0)
     -- print (a,b)
     -- let labelMap = read labelMapStr :: IntMap Word16
     -- -- runResourceT $
     -- --   hwdbSource path $$ offlineCharacterConduit labelMap =$= extractRangeConduit (1,1) =$= testSink
     -- imgs <-
     --   runResourceT $ hwdbSource path $$ offlineCharacterConduit labelMap =$= extractRangeConduit (2,2) =$= CL.take 1
     -- plotCharacter "test.png" . L.head $ imgs
     imgs <-
       runResourceT $
       CB.sourceFile path $$ sparseOfflineCharacterConduit =$= extractRangeSparseConduit (2,2) =$= CL.take 1
     print . L.length $ imgs
     plotSparseCharacter "test.png" . L.last $ imgs
     -- (s,ss) <-
     --   runResourceT $
     --   CB.sourceFile path $$ sparseOfflineCharacterConduit =$=
     --   CL.fold (\(s',ss') (SparseOfflineCharacter _ w h c) ->
     --              (s' + (fromIntegral . VU.length $ c)
     --              ,ss' + (fromIntegral w * fromIntegral h)))
     --           (0 :: Double,0 :: Double)
     -- print $ s / ss
