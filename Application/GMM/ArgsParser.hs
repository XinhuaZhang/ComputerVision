{-# LANGUAGE TemplateHaskell #-}

module Application.GMM.ArgsParser where

import           CV.CUDA.DataType
import           Data.List             as L
import           Data.Maybe
import           System.Console.GetOpt
import           System.Environment
import           Text.Read

data Flag
  = InputFile String
  | LabelFile String
  | Thread Int
  | C Double
  | ModelName String
  | FindC
  | BatchSize Int
  | GPUId [Int]
  | GPUDataType String
  | DownsampleFactor Int
  | GMMFile String
  | Threshold Double
  | NumGaussian Int
  | Freq Int
  | Scale [Double]
  deriving (Show)

data Params = Params
  { inputFile        :: String
  , labelFile        :: String
  , c                :: Double
  , numThread        :: Int
  , modelName        :: String
  , findC            :: Bool
  , batchSize        :: Int
  , gpuId            :: [Int]
  , gpuDataType      :: GPUDataType
  , downsampleFactor :: Int
  , gmmFile          :: String
  , threshold        :: Double
  , numGaussian      :: Int
  , freq             :: Int
  , scale            :: [Double]
  } deriving (Show)

options :: [OptDescr Flag]
options =
  [Option ['i']
          ["inputfile"]
          (ReqArg InputFile "FILE")
          "Image path list file."
  ,Option ['l']
          ["label"]
          (ReqArg LabelFile "FILE")
          "Input label file"
  ,Option ['c']
          ["constrainC"]
          (ReqArg (\x -> C $ readDouble x) "Double")
          "Set the liblinear parameter c (Default 1)"
  ,Option ['t']
          ["thread"]
          (ReqArg (\x -> Thread $ readInt x) "INT")
          "Set the number of threads as x (\"+RTS -Nx\" should be added at the end of the command)"
  ,Option ['C']
          ["findC"]
          (NoArg FindC)
          "Find parameter C. You may want to specify the initial c value using -c. The default initial c value is 1. Set it to be -1 to let the problem to find a initial value for c"
  ,Option ['b']
          ["batchSize"]
          (ReqArg (\x -> BatchSize $ readInt x) "INT")
          "Set the batchSize."
  ,Option ['d']
          ["gpuId"]
          (ReqArg (\x ->
                     let go [] = []
                         go (y:ys) =
                           if y == ','
                              then go ys
                              else [y] : go ys
                     in GPUId $ map readInt $ go x)
                  "[INT]")
          "Set GPU ID"
  ,Option ['m']
          ["modelName"]
          (ReqArg ModelName "NAME")
          "SVM model name."
  ,Option ['g']
          ["gpuDataType"]
          (ReqArg GPUDataType "NAME")
          "GPU datatype."
  ,Option ['f']
          ["downsampleFactor"]
          (ReqArg (\x -> DownsampleFactor $ readInt x) "INT")
          "Set the DownsampleFactor (Default 1)"
  ,Option ['z']
          ["GMMFile"]
          (ReqArg GMMFile "FILE")
          "Tree data file."
  ,Option ['h']
          ["threshold"]
          (ReqArg (\x -> Threshold $ readDouble x) "DOUBLE")
          "Set the stoppint criteria. It is the percentage that the probability increases. If it is lower than the threshold, then the program stops."
  ,Option ['n']
          ["numGaussian"]
          (ReqArg (\x -> NumGaussian $ readInt x) "INT")
          "Set the number of Gaussian in GMM."
  ,Option ['a']
          ["freq"]
          (ReqArg (\x -> Freq $ readInt x) "INT")
          "Set the radial and angular frequencies. Their ranges are assumed to be the same."
  ,Option ['e']
          ["scale"]
          (ReqArg (\x ->
                     let go xs [] = [xs]
                         go xs (y:ys) =
                           if y == ','
                              then xs : (go [] ys)
                              else go (y:xs) ys
                     in Scale $ map (readDouble . L.reverse) $ go [] x)
                  "[Double]")
          "Set the scale list"]

readInt :: String -> Int
readInt str =
  case (readMaybe str :: Maybe Int) of
    Nothing -> error $ "\nRead integer error: " ++ str
    Just x  -> x

readDouble :: String -> Double
readDouble str =
  case (readMaybe str :: Maybe Double) of
    Nothing -> error $ "\nRead double error: " ++ str
    Just x  -> x

compilerOpts :: [String] -> IO [Flag]
compilerOpts argv =
  case getOpt Permute options argv of
    (o, [], [])      -> return o
    (_, nonOpts, []) -> error $ "unrecognized arguments: " ++ unwords nonOpts
    (_, _, errs)     -> error (concat errs ++ usageInfo header options)
  where
    header = "Usage: ic [OPTION...] files..."


parseFlag :: [Flag] -> Params
parseFlag flags = go flags defaultFlag
  where defaultFlag =
          Params {inputFile = ""
                 ,labelFile = ""
                 ,c = 1.0
                 ,numThread = 1
                 ,modelName = "model"
                 ,findC = False
                 ,batchSize = 1
                 ,gpuId = [0]
                 ,gpuDataType = GPUFloat
                 ,downsampleFactor = 1
                 ,gmmFile = "gmm.dat"
                 ,threshold = -15
                 ,numGaussian = 1
                 ,freq = 0
                 ,scale = [1]}
        go [] params = params
        go (x:xs) params =
          case x of
            InputFile str -> go xs (params {inputFile = str})
            LabelFile str -> go xs (params {labelFile = str})
            Thread n -> go xs (params {numThread = n})
            C v -> go xs (params {c = v})
            ModelName str -> go xs (params {modelName = str})
            FindC -> go xs (params {findC = True})
            BatchSize x -> go xs (params {batchSize = x})
            GPUId x -> go xs (params {gpuId = x})
            GPUDataType x ->
              go xs (params {gpuDataType = read x :: GPUDataType})
            DownsampleFactor v -> go xs (params {downsampleFactor = v})
            GMMFile str -> go xs (params {gmmFile = str})
            Threshold v -> go xs (params {threshold = v})
            NumGaussian v -> go xs (params {numGaussian = v})
            Freq v -> go xs (params {freq = v})
            Scale v -> go xs (params {scale = v})

parseArgs :: [String] -> IO Params
parseArgs args = do
  flags <- compilerOpts args
  return $ parseFlag flags
