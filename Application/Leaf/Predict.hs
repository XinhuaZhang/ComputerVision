import           Application.FacialExpression.Conduit
import           Classifier.LibLinear
import           Control.Monad                        as M
import           Control.Monad.Trans.Resource
import           CV.IO.ImageIO
import           CV.Utility.Parallel
import           CV.V4Filter
import           Data.Conduit
import           Data.Conduit.List                    as CL
import           Data.List                            as L
import           Data.Set                             as S
import           Data.Vector.Unboxed                  as VU
import           System.Environment
import           CV.Array.Image
import Data.Array.Repa as R

main = do
  (imageListPath:labelListPath:isColorStr:paramsFilePath:_) <- getArgs
  v4QuardTreeFilterParams <-
    fmap (\x -> read x :: V4QuadTreeSeparableFilterParams) . readFile $
    paramsFilePath
  let parallelParams = ParallelParams {numThread = 12, batchSize = 120}
      filterVecsList =
        generateV4SeparableFilterQuadTreeFilter v4QuardTreeFilterParams
      n = 128
      downsampleFactor = 1
      isColor = read isColorStr :: Bool
  runResourceT $
    imagePathSource imageListPath $$ readImageConduit isColor =$=
    -- CL.map (R.map (\x -> let y = 255 - x
    --                      in if y > 50
    --                            then 1
    --                            else 0)) =$=
    -- CL.map (R.map (\x -> 255 - x)) =$=
    padResizeImageConduit parallelParams n 255 =$=
    applyV4QuadTreeFilterConduit parallelParams filterVecsList =$=
    CL.map (normalizeVec . VU.concat . L.map VU.concat) =$=
    featureConduitP parallelParams =$=
    mergeSource (labelSource labelListPath) =$=
    predict "SVM_model" "SVM_model.out"
