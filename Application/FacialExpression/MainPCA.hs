import           Application.FacialExpression.Conduit
import           Application.FacialExpression.PCA
import           Classifier.LibLinear
import           Control.Monad                        as M
import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           CV.IO.ImageIO
import           CV.Utility.Parallel
import           CV.V4Filter
import           Data.Conduit
import           Data.Conduit.List                    as CL
import           Data.List                            as L
import           Data.Set                             as S
import           Data.Vector.Unboxed                  as VU
import           System.Environment

main =
  do (path:_) <- getArgs
     let parallelParams =
           ParallelParams {numThread = 12
                          ,batchSize = 480}
         polarSeparableFilterParamsSet =
           PolarSeparableFilterParamsSet {getSizeSet = (n,n)
                                         ,getDownsampleFactorSet =
                                            downsampleFactor
                                         ,getScaleSet =
                                            S.fromDistinctAscList [16]
                                         ,getRadialFreqSet =
                                            S.fromDistinctAscList [0 .. (16 - 1)]
                                         ,getAngularFreqSet =
                                            S.fromDistinctAscList [0 .. (8 - 1)]
                                         ,getNameSet = Pinwheels}
         polarSeparableFilterParamsSet1 =
           PolarSeparableFilterParamsSet {getSizeSet = (n,n)
                                         ,getDownsampleFactorSet =
                                            downsampleFactor
                                         ,getScaleSet =
                                            S.fromDistinctAscList [8,12]
                                         ,getRadialFreqSet =
                                            S.fromDistinctAscList [0 .. (8 - 1)]
                                         ,getAngularFreqSet =
                                            S.fromDistinctAscList [0 .. (8 - 1)]
                                         ,getNameSet = Pinwheels}
         cartesianGratingFilterParams =
           CartesianGratingFilterParams {getCartesianGratingFilterRows = n
                                        ,getCartesianGratingFilterCols = n
                                        ,getCartesianGratingFilterDownsampleFactor =
                                           downsampleFactor
                                        ,getCartesianGratingFilterScale = [24]
                                        ,getCartesianGratingFilterFreq =
                                           [0.125,0.25,0.5,1]
                                        ,getCartesianGratingFilterAngle =
                                           [0,10 .. 180 - 10] -- [0, 45, 90, 135]
                                         }
         hyperbolicFilterParams =
           HyperbolicFilterParams {getHyperbolicFilterRows = n
                                  ,getHyperbolicFilterCols = n
                                  ,getHyperbolicFilterDownsampleFactor =
                                     downsampleFactor
                                  ,getHyperbolicFilterScale = [24]
                                  ,getHyperbolicFilterFreq = [0.125,0.25,0.5,1]
                                  ,getHyperbolicFilterAngle = [0,10 .. 90 - 10] --[0, 45, 90]
                                   }
         polarSeparableFilter =
           getFilterVectors
             (makeFilterGrid gridSize $
              PolarSeparableFilter polarSeparableFilterParamsSet [] :: PolarSeparableFilterExpansion)
         polarSeparableFilter1 =
           getFilterVectors
             (makeFilterGrid gridSize $
              PolarSeparableFilter polarSeparableFilterParamsSet1 [] :: PolarSeparableFilterExpansion)
         cartesianGratingFilter =
           getFilterVectors
             (makeFilterGrid gridSize $
              CartesianGratingFilter cartesianGratingFilterParams [] :: CartesianGratingFilter)
         hyperbolicFilter =
           getFilterVectors
             (makeFilterGrid gridSize $ HyperbolicFilter hyperbolicFilterParams [] :: HyperbolicFilter)
         filters =
           [polarSeparableFilter,cartesianGratingFilter,hyperbolicFilter]
         n = 128
         downsampleFactor = 1
         gridSize = (16,16)
     labels <- runResourceT $ labelSource' path $$ CL.consume
     landmarks <- runResourceT $ landmarksSource path $$ CL.consume
     features <-
       runResourceT $
       filePathSource path $$ readImageConduit False =$=
       mergeSource (CL.sourceList landmarks) =$=
       cropSquareConduit parallelParams n =$=
       applyFilterFixedSizeConduit parallelParams downsampleFactor filters =$=
       CL.consume
     let trainParams =
           TrainParams {trainSolver = L2R_L2LOSS_SVC_DUAL
                       ,trainC = 128
                       ,trainNumExamples = L.length features
                       ,trainFeatureIndexMax =
                          (L.sum . L.map L.length $ filters) * 2
                       ,trainModel = "SVM_model"}
     print trainParams
     crossValidation parallelParams
                     trainParams
                     8
                     (L.zip (L.map fromIntegral labels) features)
