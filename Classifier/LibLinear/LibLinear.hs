{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PatternGuards              #-}

module MachineLearning.Classifier.LibLinear.LibLinear ( TrainParams(..)
                                                      , Solver(..)
                                                      , Feature(..)
                                                      , readLabelFile
                                                      , labelSource
                                                      , featureNode
                                                      , train
                                                      , predict
                                                      , findParameterC
                                                      , problem
                                                      , problemMaxInd
                                                      , libLinearRead
                                                      , libLinearWrite
                                                      , debugSink) where
import           Classifier.LibLinear.Bindings
import           Classifier.LibLinear.Solver
import qualified Control.Monad                 as M
import           Control.Monad.IO.Class        (liftIO)
import           Data.Binary.Get
import           Data.Binary.IEEE754
import           Data.Binary.Put
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy          as BL
import           Data.Conduit
import qualified Data.Conduit.List             as CL
import           Data.Either
import qualified Data.Foldable                 as F
import qualified Data.List                     as L
import           Data.Maybe
import qualified Data.Sequence                 as S
import qualified Data.Vector                   as V
import qualified Data.Vector.Storable.Mutable  as MVec
import qualified Data.Vector.Unboxed           as VU
import           Foreign                       as F
import           Foreign.C.String
import           Foreign.C.Types
import           GHC.Float
import           System.IO
import           System.Mem



{-Syntax-}
type Feature = (Int,Double)

data TrainParams = TrainParams
  { trainSolver      :: Solver
  , trainC           :: Double
  , trainNumExamples :: Int
  , trainModel       :: String
  } deriving (Show)

{-Error Information-}
data LibLinearError  = InvalidLabel | WrongNumExamples Int deriving (Show)

{-Functions for Label-}
readLabel    :: B.ByteString -> Either LibLinearError Int
readLabel bs
  | isNothing label               = Left InvalidLabel
  | B.null $ snd $ fromJust label = Right $ fst $ fromJust label
  | otherwise                     = Left InvalidLabel
  where label = B.readInt bs

readLabelFile          :: FilePath -> IO [Int]
readLabelFile filePath = do h <- openFile filePath ReadMode
                            bs <- B.hGetContents h
                            let bsList = B.lines bs
                            hClose h
                            return $ map read bsList
  where read x  = case readLabel x of
                    Left msg -> error $ show msg
                    Right label -> label

labelSource          :: FilePath -> Source IO Double
labelSource filePath = do h <- liftIO $ openFile filePath ReadMode
                          !bs <- liftIO $ B.hGetContents h
                          let bsList = B.lines bs
                          M.mapM_ (yield.fromIntegral.read) bsList
                          liftIO $ hClose h
  where read x  = case readLabel x of
                    Left msg -> error $ show msg
                    Right label -> label

{-Parameter Functions-}
newParameter :: Solver -> Double -> C'parameter
newParameter solver c = C'parameter
  { c'parameter'solver_type = fromIntegral $ fromEnum solver
  , c'parameter'eps = 0.1
  , c'parameter'C = realToFrac c
  , c'parameter'nr_weight = 0
  , c'parameter'weight_label = nullPtr
  , c'parameter'weight = nullPtr
  , c'parameter'p = 0.1
  , c'parameter'init_sol = nullPtr
  }

{-Problem Functions-}
featuresToNodeList
  :: VU.Vector Feature -> V.Vector C'feature_node
featuresToNodeList features =
  V.snoc (V.map mapper (VU.convert features)) sentintel
  where mapper (i,v) =
          C'feature_node {c'feature_node'index = fromIntegral i
                         ,c'feature_node'value = realToFrac v}
        sentintel = mapper $ ((-1),0.0)

labelToTargetList
  :: [Int] -> MVec.IOVector CDouble -> IO ()
labelToTargetList label vec =
  M.zipWithM_
    (\i x ->
       MVec.write vec
                  i
                  (fromIntegral x))
    [0 ..]
    label

updateFeatureListRankOne :: V.Vector C'feature_node
                         -> MVec.IOVector C'feature_node
                         -> IO ()
updateFeatureListRankOne xs vec =
  V.zipWithM_
    (\i x -> MVec.write vec i x)
    (V.generate (V.length xs)
                (\i -> i))
    xs

updateFeatureNodeList
  :: MVec.IOVector (Ptr C'feature_node)
  -> MVec.IOVector CDouble
  -> (Int,Int, S.Seq (MVec.IOVector C'feature_node))
  -> (Double,Int,MVec.IOVector C'feature_node)
  -> IO (Int,Int, S.Seq (MVec.IOVector C'feature_node))
updateFeatureNodeList !featureIndex !targetsVec (!i,!maxInd,!vecs') (!label,!maxInd',!vec) =
  do let maxIndex = max maxInd maxInd'
     MVec.unsafeWith vec
                     (\basePtr -> MVec.write featureIndex i basePtr)
     MVec.write targetsVec
                i
                (realToFrac label)
     return (i + 1,maxIndex,  vecs' S.|> vec)

problemMaxInd
  :: Conduit (Double,VU.Vector Feature) IO (Double,Int,MVec.IOVector C'feature_node)
problemMaxInd =
  awaitForever
    (\(label,x) ->
       let !maxInd = (VU.maximum . fst . VU.unzip) x
       in do !vec <- liftIO $ MVec.new $ (VU.length x + 1)
             liftIO $
               updateFeatureListRankOne (featuresToNodeList x)
                                        vec
             yield (label,maxInd,vec))

problem
  :: Conduit (Double,VU.Vector Feature) IO (Double,MVec.IOVector C'feature_node)
problem =
  awaitForever
    (\(label,x) ->
       do !vec <- liftIO $ MVec.new $ (VU.length x + 1)
          liftIO $
            updateFeatureListRankOne (featuresToNodeList x)
                                     vec
          yield (label,vec))

featureNode
  :: Conduit (VU.Vector Feature) IO (MVec.IOVector C'feature_node)
featureNode =
  awaitForever
    (\x ->
       do !vec <- liftIO $ MVec.new $ (VU.length x + 1)
          liftIO $
            updateFeatureListRankOne (featuresToNodeList x)
                                     vec
          yield vec)

{-Train and Predict Functions-}
extractPtr          :: [F.ForeignPtr a] -> IO b -> IO b
extractPtr [] f     = f
extractPtr (x:xs) f = F.withForeignPtr x (\_ -> extractPtr xs f)


train
  :: TrainParams
  -> Sink (Double,Int,MVec.IOVector C'feature_node) IO (Either LibLinearError (IO ()))
train TrainParams{trainSolver,trainC,trainNumExamples,trainModel} =
  do !targets <- liftIO $ MVec.new $ trainNumExamples
     !featureIndex <- liftIO $ MVec.new trainNumExamples
     !(count,trainFeatureMax,vecs) <-
       CL.foldM (updateFeatureNodeList featureIndex targets)
                (0,0,S.empty)
     if count /= trainNumExamples
        then return $ Left (WrongNumExamples count)
        else return $
             Right $
             extractPtr
               (map (\x -> fst $ MVec.unsafeToForeignPtr0 x)
                    (F.toList vecs))
               (MVec.unsafeWith targets $
                \targets' ->
                  MVec.unsafeWith featureIndex $
                  \features' ->
                    do let p =
                             C'problem {c'problem'l =
                                          fromIntegral trainNumExamples
                                       ,c'problem'n =
                                          fromIntegral trainFeatureMax
                                       ,c'problem'y = targets'
                                       ,c'problem'x = features'
                                       ,c'problem'bias = -1.0}
                       model <-
                         with p $
                         \problem' ->
                           with (newParameter trainSolver trainC) $
                           \param' -> c'train problem' param'
                       modelName <- newCString trainModel
                       c'save_model modelName model)

--train' :: TrainParams
--       -> MVec.IOVector CDouble
--       -> MVec.IOVector (Ptr C'feature_node)
--       -> (Int,Int, S.Seq (MVec.IOVector C'feature_node))
--       -> Int
--       -> Int
--       -> Sink (Double,Int, MVec.IOVector C'feature_node) IO ()
--train' TrainParams{trainSolver,trainC,trainNumExamples,trainModel} targets featureIndex (count',trainFeatureMax',vecs') trunkSize n =
--  do !trunk <- CL.take trunkSize
--     if length trunk == 0
--        then return ()
--        else do !(count,trainFeatureMax,vecs) <-
--                  liftIO $
--                  F.foldlM (updateFeatureNodeList featureIndex targets)
--                           (count',trainFeatureMax',vecs')
--                           trunk
--                liftIO $
--                  extractPtr
--                    (map (\x -> fst $ MVec.unsafeToForeignPtr0 x)
--                         (F.toList vecs))
--                    (MVec.unsafeWith targets $
--                     \targets' ->
--                       MVec.unsafeWith featureIndex $
--                       \features' ->
--                         do let p =
--                                  C'problem {c'problem'l =
--                                               fromIntegral (n * trunkSize)
--                                            ,c'problem'n =
--                                               fromIntegral trainFeatureMax
--                                            ,c'problem'y = targets'
--                                            ,c'problem'x = features'
--                                            ,c'problem'bias = -1.0}
--                            model <-
--                              with p $
--                              \problem' ->
--                                with (newParameter trainSolver trainC) $
--                                \param' -> c'train problem' param'
--                            modelName <-
--                              newCString
--                                (show (n * trunkSize) ++ "_" ++ trainModel)
--                            c'save_model modelName model)
--                train' TrainParams {trainSolver
--                                   ,trainC
--                                   ,trainNumExamples
--                                   ,trainModel}
--                       targets
--                       featureIndex
--                       (count,trainFeatureMax,vecs)
--                       trunkSize
--                       (n + 1)




predict
  :: String -> FilePath -> Sink (Double,MVec.IOVector C'feature_node) IO ()
predict predictModel output =
  do modelName <- liftIO $ newCString predictModel
     model <- liftIO $ c'load_model modelName
     (correct,total) <-
       CL.foldM (func model)
                (0,0)
     let percent = (fromIntegral correct) / (fromIntegral total) * 100
         str = show percent
     liftIO $ putStrLn str
     h <- liftIO $ openFile output WriteMode
     liftIO $ hPutStrLn h str
     liftIO $ hClose h
  where func :: Ptr C'model
             -> (Int,Int)
             -> (Double,MVec.IOVector C'feature_node)
             -> IO (Int,Int)
        func model (correct,total) (!t,!vec) =
          do prediction <- MVec.unsafeWith vec $ \vec' -> c'predict model vec'
             if realToFrac t == prediction
                then return (correct + 1,total + 1)
                else return (correct,total + 1)


{-Cross validation -}
findParameterC
  :: TrainParams
  -> Sink (Double,Int,MVec.IOVector C'feature_node) IO (Either LibLinearError (IO ()))
findParameterC TrainParams{trainSolver,trainC,trainNumExamples,trainModel} =
  do targets <- liftIO $ MVec.new $ trainNumExamples
     featureIndex <- liftIO $ MVec.new trainNumExamples
     (count,trainFeatureMax,vecs) <-
       CL.foldM (updateFeatureNodeList featureIndex targets)
                (0,0,S.empty)
     if count /= trainNumExamples
        then return $ Left (WrongNumExamples count)
        else return $
             Right $
             extractPtr
               (map (\x -> fst $ MVec.unsafeToForeignPtr0 x)
                    (F.toList vecs))
               (MVec.unsafeWith targets $
                \targets' ->
                  MVec.unsafeWith featureIndex $
                  \features' ->
                    do let p =
                             C'problem {c'problem'l =
                                          fromIntegral trainNumExamples
                                       ,c'problem'n =
                                          fromIntegral trainFeatureMax
                                       ,c'problem'y = targets'
                                       ,c'problem'x = features'
                                       ,c'problem'bias = -1.0}
                       with p $
                         \problem' ->
                           with (newParameter trainSolver trainC) $
                           \param' ->
                             F.allocaBytes 8 $
                             \bestC' ->
                               F.allocaBytes 8 $
                               \bestRate' ->
                                 do c'find_parameter_C problem'
                                                       param'
                                                       5
                                                       (realToFrac trainC)
                                                       1024
                                                       bestC'
                                                       bestRate'
                                    bestC <- F.peek bestC'
                                    bestRate <- F.peek bestRate'
                                    putStrLn $
                                      "Best C = " ++
                                      show bestC ++
                                      " CV accuracy = " ++
                                      show (100 * bestRate) ++ "%")


{-IO Functions-}
putFeautre :: (Double,[Feature]) -> Put
putFeautre (label,feature) =
  do putFloat32le $ double2Float label
     M.mapM_ (\(i,v) ->
                do putWord32le (fromIntegral i)
                   putFloat32le $ double2Float v)
             feature

libLinearWrite
  :: FilePath -> Int -> Sink (Double,[Feature]) IO ()
libLinearWrite filePath len =
  do handle <- liftIO $ openBinaryFile filePath WriteMode
     liftIO $ BL.hPut handle $ runPut $ putWord32le (fromIntegral len)
     go handle
  where go h =
          do node <- await
             case node of
               Nothing -> liftIO $ hClose h
               Just (label,feature) ->
                 do liftIO $
                      BL.hPut h $
                      runPut $ putWord32le $ fromIntegral $ length feature
                    liftIO $ BL.hPut h $ runPut $ putFeautre (label,feature)
                    go h

getLength :: Get Int
getLength =
  do len <- getWord32le
     return $ fromIntegral len

getFeature :: Get Feature
getFeature =
  do index <- getWord32le
     value <- getFloat32le
     return (fromIntegral index,float2Double value)

getNode :: Int -> Get (Double,VU.Vector Feature)
getNode len =
  do label <- getFloat32le
     feature <- M.replicateM len getFeature
     return (float2Double label,VU.fromList feature)

libLinearRead
  :: FilePath -> Source IO (Double,VU.Vector Feature)
libLinearRead filePath =
  do handle <- liftIO $ openBinaryFile filePath ReadMode
     lenBs <- liftIO $ BL.hGet handle 4
     let len = runGet getLength lenBs
     go handle len
  where go h n =
          if n > 0
             then do featureLenBs <- liftIO $ BL.hGet h 4
                     let featureLen = runGet getLength featureLenBs
                     bs <- liftIO $ BL.hGet h (4 + 8 * featureLen)
                     let (label,vec) = runGet (getNode featureLen) bs
                     yield (label,vec)
                     go h (n - 1)
             else liftIO $ hClose h


debugSink
  :: Sink (Double,VU.Vector Feature) IO ()
debugSink =
  do x <- await
     case x of
       Just (label,vec) ->
         do liftIO $ print label
            liftIO $ print vec
       Nothing -> liftIO $ return ()
