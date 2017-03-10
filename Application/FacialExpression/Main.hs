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
           ParallelParams {numThread = 4
                          ,batchSize = 40}
         filterParamsSet =
           PolarSeparableFilterParamsSet {getSizeSet = (0,0)
                                         ,getDownsampleFactorSet = 1
                                         ,getScaleSet =
                                            S.fromDistinctAscList [4,8,16,20]
                                         ,getRadialFreqSet =
                                            S.fromDistinctAscList [0 .. (16 - 1)]
                                         ,getAngularFreqSet =
                                            S.fromDistinctAscList [0 .. (16 - 1)]
                                         ,getNameSet = Pinwheels}
     labels <- runResourceT $ labelSource' path $$ CL.consume
     landmarks <- runResourceT $ landmarksSource path $$ CL.consume
     featurePtr <-
       runResourceT $
       filePathSource path $$ readImageConduit False =$=
       mergeSource (CL.sourceList landmarks) =$=
       cropConduit parallelParams =$=
       applyFilterCenterVariedSizeConduit parallelParams filterParamsSet =$=
       -- CL.map complexVec2RealVec =$=
       featurePtrConduitP parallelParams =$=
       CL.consume
     let trainParams =
           TrainParams {trainSolver = L2R_L2LOSS_SVC_DUAL
                       ,trainC = 0.125
                       ,trainNumExamples = L.length featurePtr
                       ,trainFeatureIndexMax =
                          (getFilterNum filterParamsSet) * 2
                       ,trainModel = "SVM_model"}
     print trainParams
     findParameterC trainParams
                    (L.map fromIntegral labels)
                    featurePtr
