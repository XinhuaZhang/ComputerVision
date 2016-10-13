{-# LANGUAGE TemplateHaskell #-}

module Application.FilterStatistics.ArgsParser where

import           CV.CUDA.DataType
import           Data.List             as L
import           Data.Maybe
import           System.Console.GetOpt
import           System.Environment
import           Text.Read

data Flag
  = InputFile String
  | OutputFile String
  | Thread Int
  | BatchSize Int
  | GPUId [Int]
  | GPUDataType String
  deriving (Show)

data Params = Params
  { inputFile :: String
  , outputFile :: String
  , numThread :: Int
  , batchSize :: Int
  , gpuId :: [Int]
  , gpuDataType :: GPUDataType
  } deriving (Show)

options :: [OptDescr Flag]
options =
  [ Option ['i'] ["inputfile"] (ReqArg InputFile "FILE") "Image path list file."
  , Option ['o'] ["output"] (ReqArg OutputFile "FILE") "Output file"
  , Option
      ['t']
      ["thread"]
      (ReqArg (\x -> Thread $ readInt x) "INT")
      "Set the number of threads as x (\"+RTS -Nx\" should be added at the end of the command)"
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
  , Option ['g'] ["gpuDataType"] (ReqArg GPUDataType "NAME") "GPU datatype."
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
      , outputFile = ""
      , numThread = 1
      , batchSize = 1
      , gpuId = [0]
      , gpuDataType = GPUFloat
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
        OutputFile str ->
          go
            xs
            (params
             { outputFile = str
             })
        Thread n ->
          go
            xs
            (params
             { numThread = n
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

parseArgs :: [String] -> IO Params
parseArgs args = do
  flags <- compilerOpts args
  return $ parseFlag flags
