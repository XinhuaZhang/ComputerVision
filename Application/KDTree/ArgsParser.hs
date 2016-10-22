{-# LANGUAGE TemplateHaskell #-}

module Application.KDTree.ArgsParser where

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
  | Radius Double
  | DownsampleFactor Int
  | TreeFile String
  | SampleRate Int
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
  , radius           :: Double
  , downsampleFactor :: Int
  , treeFile         :: String
  , sampleRate       :: Int
  } deriving (Show)

options :: [OptDescr Flag]
options =
  [ Option ['i'] ["inputfile"] (ReqArg InputFile "FILE") "Image path list file."
  , Option ['l'] ["label"] (ReqArg LabelFile "FILE") "Input label file"
  , Option
      ['c']
      ["constrainC"]
      (ReqArg (\x -> C $ readDouble x) "Double")
      "Set the liblinear parameter c (Default 1)"
  , Option
      ['t']
      ["thread"]
      (ReqArg (\x -> Thread $ readInt x) "INT")
      "Set the number of threads as x (\"+RTS -Nx\" should be added at the end of the command)"
  , Option
      ['C']
      ["findC"]
      (NoArg FindC)
      "Find parameter C. You may want to specify the initial c value using -c. The default initial c value is 1. Set it to be -1 to let the problem to find a initial value for c"
  , Option
      ['b']
      ["batchSize"]
      (ReqArg (\x -> BatchSize $ readInt x) "INT")
      "Set the batchSize."
  , Option
      ['d']
      ["gpuId"]
      (ReqArg
         (\x ->
             let go [] = []
                 go (y:ys) =
                   if y == ','
                     then go ys
                     else [y] : go ys
             in GPUId $ map readInt $ go x)
         "[INT]")
      "Set GPU ID"
  , Option ['m'] ["modelName"] (ReqArg ModelName "NAME") "SVM model name."
  , Option ['g'] ["gpuDataType"] (ReqArg GPUDataType "NAME") "GPU datatype."
  , Option
      ['r']
      ["radius"]
      (ReqArg (\x -> Radius $ readDouble x) "Double")
      "Set the kdtree bolb radius (Default 10)"
  , Option
      ['f']
      ["downsampleFactor"]
      (ReqArg (\x -> DownsampleFactor $ readInt x) "INT")
      "Set the DownsampleFactor (Default 1)"
  , Option ['z'] ["treeFile"] (ReqArg TreeFile "FILE") "Tree data file."
  , Option
      ['s']
      ["sampleRate"]
      (ReqArg (\x -> SampleRate $ readInt x) "INT")
      "Set the sampleRate."
  ]

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
  where
    defaultFlag =
      Params
      { inputFile = ""
      , labelFile = ""
      , c = 1.0
      , numThread = 1
      , modelName = "model"
      , findC = False
      , batchSize = 1
      , gpuId = [0]
      , gpuDataType = GPUFloat
      , radius = 10
      , downsampleFactor = 1
      , treeFile = "featurePoints.dat"
      , sampleRate = 11
      }
    go [] params = params
    go (x:xs) params =
      case x of
        InputFile str ->
          go
            xs
            (params
             { inputFile = str
             })
        LabelFile str ->
          go
            xs
            (params
             { labelFile = str
             })
        Thread n ->
          go
            xs
            (params
             { numThread = n
             })
        C v ->
          go
            xs
            (params
             { c = v
             })
        ModelName str ->
          go
            xs
            (params
             { modelName = str
             })
        FindC ->
          go
            xs
            (params
             { findC = True
             })
        BatchSize x ->
          go
            xs
            (params
             { batchSize = x
             })
        GPUId x ->
          go
            xs
            (params
             { gpuId = x
             })
        GPUDataType x ->
          go
            xs
            (params
             { gpuDataType = read x :: GPUDataType
             })
        Radius v ->
          go
            xs
            (params
             { radius = v
             })
        DownsampleFactor v ->
          go
            xs
            (params
             { downsampleFactor = v
             })
        TreeFile str ->
          go
            xs
            (params
             { treeFile = str
             })
        SampleRate x -> go xs (params {sampleRate = x})

parseArgs :: [String] -> IO Params
parseArgs args = do
  flags <- compilerOpts args
  return $ parseFlag flags
