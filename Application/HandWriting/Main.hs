import           Application.HandWriting.Conduit
import           Application.HandWriting.IO
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Conduit.List               as CL
import           Data.List                       as L
import           Data.Vector.Unboxed             as VU
import           System.Environment


main =
  do (path:_) <- getArgs
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
     runResourceT $
       hwdbSource path $$ offlineCharacterConduit =$= testSink
     imgs <-
       runResourceT $ hwdbSource path $$ offlineCharacterConduit =$= CL.take 1
     plotCharacter "test.png" . L.head $ imgs
