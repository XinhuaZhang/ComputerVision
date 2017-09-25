import           Application.ShiftablePinwheelPyramid.ArgsParser as AP
import           Application.ShiftablePinwheelPyramid.Conduit
import           Control.Monad                                   as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.IO.ImageIO
import           CV.Utility.Parallel                             as Par
import           Data.Array.Repa                                 as R
import           Data.Conduit
import           Data.Conduit.Binary                             as CB
import           Data.Conduit.List                               as CL
import           Data.List                                       as L
import           System.Environment

main = do
  args <- getArgs
  params <- parseArgs args
  print params
  let parallelParams =
        ParallelParams
        { Par.numThread = AP.numThread params
        , Par.batchSize = AP.batchSize params
        }
      centers = [(48,64)]
        -- [ (i, j)
        -- | i <- generateCenters (imageSize params) (numGrid params)
        -- , j <- generateCenters (imageSize params) (numGrid params) ]
  print centers
  xs <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    logpolarImageConduit
      parallelParams
      64
      64
      centers
      (radius params)
      (logpolarFlag params) =$=
    CL.take (numGMMExample params)
  M.zipWithM_
    (\i (_, arr) -> do
       let (Z :. np :. _ :. _ :. _) = extent arr
       M.mapM_
         (\j ->
             plotImageRepa (show i L.++ "_" L.++ show j L.++ ".png") .
             Image 8 . computeUnboxedS . R.slice arr $
             (Z :. j :. All :. All :. All))
         [0 .. np-1])
    [1 ..]
    xs
