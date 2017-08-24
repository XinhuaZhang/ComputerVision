module Application.STL10.IO where

import           Control.Monad                as M
import           Control.Monad.Trans.Resource
import           Data.Array.Repa              as R
import           Data.ByteString              as BS
import           Data.ByteString.Lazy         as BL
import           Data.Conduit
import           Data.Conduit.Binary          as CB
import           Data.List                    as L

readSTL10ImageConduit :: Conduit BS.ByteString (ResourceT IO) (R.Array U DIM3 Double)
readSTL10ImageConduit = do
  bs <- CB.take 27648
  unless
    (BL.null bs)
    (do yield . fromListUnboxed (Z :. 3 :. 96 :. 96) . L.map fromIntegral . BL.unpack $
          bs
        readSTL10ImageConduit)

readSTL10LabelConduit :: Conduit BS.ByteString (ResourceT IO) Int
readSTL10LabelConduit = do
  x <- CB.take 1
  unless
    (BL.null x)
    (do yield . fromIntegral . BL.head $ x
        readSTL10LabelConduit)
