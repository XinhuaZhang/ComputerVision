module Application.PairwiseEntropy.ArgsParser where

import           Data.List             as L
import           Data.Maybe
import           System.Console.GetOpt
import           Text.Read

data Flag
  = InputFile String
  | LabelFile String
  | Thread Int
  | C Double
  | ModelName String
  | FindC
  | BatchSize Int
  | DownsampleFactor Int
  | GMMFile String
  | PCAFile String
  | Threshold Double
  | NumGaussian Int
  | Freq Int
  | Scale [Double]
  | IsComplex
  | NumGMMExample Int
  | IsFixedSize
  | NumPrincipal [Int]
  | NumLayer Int
  deriving (Show)

data Params = Params
  { inputFile        :: String
  , labelFile        :: String
  , c                :: Double
  , numThread        :: Int
  , modelName        :: String
  , findC            :: Bool
  , batchSize        :: Int
  , downsampleFactor :: Int
  , gmmFile          :: String
  , pcaFile          :: String
  , threshold        :: Double
  , numGaussian      :: Int
  , freq             :: Int
  , scale            :: [Double]
  , isComplex        :: Bool
  , numGMMExample    :: Int
  , isFixedSize      :: Bool
  , numPrincipal     :: [Int]
  , numLayer         :: Int
  } deriving (Show)

options :: [OptDescr Flag]
options =
  [ Option ['i'] ["inputfile"] (ReqArg InputFile "FILE") "Image path list file."
  , Option ['l'] ["label"] (ReqArg LabelFile "FILE") "Input label file"
  , Option
      ['c']
      ["constrainC"]
      (ReqArg (C . readDouble) "Double")
      "Set the liblinear parameter c (Default 1)"
  , Option
      ['t']
      ["thread"]
      (ReqArg (Thread . readInt) "INT")
      "Set the number of threads as x (\"+RTS -Nx\" should be added at the end of the command)"
  , Option
      ['C']
      ["findC"]
      (NoArg FindC)
      "Find parameter C. You may want to specify the initial c value using -c. The default initial c value is 1. Set it to be -1 to let the problem to find a initial value for c"
  , Option
      ['b']
      ["batchSize"]
      (ReqArg (BatchSize . readInt) "INT")
      "Set the batchSize."
  , Option ['m'] ["modelName"] (ReqArg ModelName "NAME") "SVM model name."
  , Option
      ['f']
      ["downsampleFactor"]
      (ReqArg (DownsampleFactor . readInt) "INT")
      "Set the DownsampleFactor (Default 1)"
  , Option ['z'] ["GMMFile"] (ReqArg GMMFile "FILE") "GMM data file."
  , Option ['z'] ["PCAFile"] (ReqArg PCAFile "FILE") "PCA data file."
  , Option
      ['h']
      ["threshold"]
      (ReqArg (Threshold . readDouble) "DOUBLE")
      "Set the stoppint criteria. It is the percentage that the probability increases. If it is lower than the threshold, then the program stops."
  , Option
      ['n']
      ["numGaussian"]
      (ReqArg (NumGaussian . readInt) "INT")
      "Set the number of Gaussian in GMM."
  , Option
      ['a']
      ["freq"]
      (ReqArg (Freq . readInt) "INT")
      "Set the radial and angular frequencies. Their ranges are assumed to be the same."
  , Option
      ['e']
      ["scale"]
      (ReqArg
         (\x ->
             let go xs [] = [xs]
                 go xs (y:ys) =
                   if y == ','
                     then xs : go [] ys
                     else go (y : xs) ys
             in Scale $ map (readDouble . L.reverse) $ go [] x)
         "[Double]")
      "Set the scale list"
  , Option
      ['z']
      ["complex"]
      (NoArg IsComplex)
      "Flag which decides using complex value or magnitude."
  , Option
      ['z']
      ["numGMMExample"]
      (ReqArg (NumGMMExample . readInt) "INT")
      "Set the number of examples which are used for GMM training."
  , Option
      ['z']
      ["fixedSize"]
      (NoArg IsFixedSize)
      "Are the images have the same sizes?"
  ,  Option
       ['z']
       ["numPrincipal"]
       (ReqArg
          (\x ->
              let go xs [] = [xs]
                  go xs (y:ys) =
                    if y == ','
                      then xs : go [] ys
                      else go (y : xs) ys
              in NumPrincipal $ map (readInt . L.reverse) $ go [] x)
          "[Int]")
       "Set the output dimension of PCA dimensional reduction."
  ,   Option
        ['l']
        ["numLayer"]
        (ReqArg (NumLayer . readInt) "INT")
        "Set the number of layers."
  ]

readInt :: String -> Int
readInt str =
  fromMaybe
    (error $ "\nRead integer error: " ++ str)
    (readMaybe str :: Maybe Int)

readDouble :: String -> Double
readDouble str =
  fromMaybe
    (error $ "\nRead double error: " ++ str)
    (readMaybe str :: Maybe Double)

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
                 ,downsampleFactor = 1
                 ,gmmFile = "gmm.dat"
                 ,pcaFile = "pca.dat"
                 ,threshold = -15
                 ,numGaussian = 1
                 ,freq = 0
                 ,scale = [1]
                 ,isComplex = False
                 ,numGMMExample = 1
                 ,isFixedSize = False
                 ,numPrincipal = [1]
                 ,numLayer = 1}
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
            DownsampleFactor v -> go xs (params {downsampleFactor = v})
            GMMFile str -> go xs (params {gmmFile = str})
            PCAFile str -> go xs (params {pcaFile = str})
            Threshold v -> go xs (params {threshold = v})
            NumGaussian v -> go xs (params {numGaussian = v})
            Freq v -> go xs (params {freq = v})
            Scale v -> go xs (params {scale = v})
            IsComplex -> go xs (params {isComplex = True})
            NumGMMExample v -> go xs (params {numGMMExample = v})
            IsFixedSize -> go xs (params {isFixedSize = True})
            NumPrincipal v -> go xs (params {numPrincipal = v})
            NumLayer x -> go xs (params {numLayer = x})

parseArgs :: [String] -> IO Params
parseArgs args = do
  flags <- compilerOpts args
  return $ parseFlag flags
