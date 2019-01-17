module CV.Utility.Conduit where

import           Control.Monad.Trans.Resource
import           Data.Conduit.List            as CL
import           Data.Conduit

takeConduit :: Int -> Conduit a (ResourceT IO) a
takeConduit n = CL.take n >>= CL.sourceList

dropConduit :: Int -> Conduit a (ResourceT IO) a
dropConduit n = CL.drop n >> CL.map id
