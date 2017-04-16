import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           CV.IO.ImageIO
import           Data.Array.Repa              as R
import           Data.Conduit                 as C
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           System.Environment
import Control.Monad as M


main = do
  (path:_) <- getArgs
  imgs <-
    runResourceT $ (sourceList [path]) $$ readImageConduit True =$= CL.take 1
  let img = L.head imgs
      params =
        ImageTransformationParams
        { imageTransformationParamsRows = 128
        , imageTransformationParamsCols = 128
        , rotationAngleParams = 90
        , scaleFactorRange = (1, 1)
        , contrastFactorARange = (0.1, 2)
        , contrastFactorBRange = (-64, 64)
        }
  paramsList <- generateImageTransformation params
  print paramsList
  plotImage "0.png" . computeS . R.map (\x -> 255 - x) $ img
  M.zipWithM_
    (\i img ->
        plotImage (show i L.++ ".png") . computeS . R.map (\x -> 255 - x) $ img)
    [1 ..] .
    padTransformImage 0 paramsList $
    img
