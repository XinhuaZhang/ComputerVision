import           Application.FacialExpression.Conduit
import           Classifier.LibLinear
import           Control.Monad                        as M
import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           CV.Filter.PolarSeparableFilter
import           CV.IO.ImageIO
import           CV.Utility.Parallel
import           Data.Conduit
import           Data.Conduit.List                    as CL
import           Data.List                            as L
import           Data.Set                             as S
import           Data.Vector.Unboxed                  as VU
import           System.Environment

main =
  do (path:_) <- getArgs
     let parallelParams =
           ParallelParams {numThread = 16
                          ,batchSize = 320}
         n = 128
     labels <- runResourceT $ labelSource' path $$ CL.consume
     landmarks <- runResourceT $ landmarksSource path $$ CL.consume
     featurePtr <-
       runResourceT $
       filePathSource path $$ readImageConduit False =$=
       mergeSource (CL.sourceList landmarks) =$=
       cropSquareConduit parallelParams n =$=
       pixelConduit parallelParams 1 =$=
       featurePtrConduitP parallelParams =$=
       CL.consume
     let trainParams =
           TrainParams {trainSolver = L2R_L2LOSS_SVC_DUAL
                       ,trainC = 0.125
                       ,trainNumExamples = L.length featurePtr
                       ,trainFeatureIndexMax = n ^ 2
                       ,trainModel = "SVM_model"}
     print trainParams
     findParameterC trainParams
                    (L.map fromIntegral labels)
                    featurePtr
