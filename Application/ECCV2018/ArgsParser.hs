module Application.ECCV2018.ArgsParser where

import           CV.Filter.PolarSeparableFilterType
import           Data.Maybe
import           System.Console.GetOpt
import           Text.Read

data Flag
  = InputFile String
  | Thread Int
  | C Double
  | ModelName String
  | OriginModelName String
  | FindC
  | BatchSize Int
  | GMMFile String
  | PCAFile String
  | Threshold Double
  | NumGaussian Int
  | NumGMMExample Int
  | NumPrincipal Int
  | ImageSize Int
  | ParamsFileName String
  | OriginPredictorParamsFileName String
  | PatchSize Int
  | Stride Int
  | NumBin Int
  | GaussianScale [Double]
  | KMeansFile String
  | DataFile String
  | FFTWWisdomFilePath FilePath
  | LogpolarFlag
  | NumGrid Int
  | Radius Double
  | LabelFile FilePath
  | NumScatteringLayer Int
  | FourierMellin
  | GaussianPinwheel
  | PinwheelFan
  | PinwheelRing
  | PinwheelBlob
  | InvariantFeature
  | CenterLength Int
  deriving (Show)

data Params = Params
  { inputFile                     :: String
  , numThread                     :: Int
  , c                             :: Double
  , modelName                     :: String
  , originModelName               :: String
  , findC                         :: Bool
  , batchSize                     :: Int
  , gmmFile                       :: String
  , pcaFile                       :: String
  , threshold                     :: Double
  , numGaussian                   :: Int
  , numGMMExample                 :: Int
  , numPrincipal                  :: Int
  , imageSize                     :: Int
  , paramsFileName                :: String
  , originPredictorParamsFileName :: String
  , patchSize                     :: Int
  , stride                        :: Int
  , numBin                        :: Int
  , gaussianScale                 :: [Double]
  , kmeansFile                    :: String
  , dataFile                      :: String
  , fftwWisdomPath                :: FilePath
  , logpolarFlag                  :: Bool
  , numGrid                       :: Int
  , radius                        :: Double
  , labelFile                     :: FilePath
  , numScatteringLayer            :: Int
  , filterType                    :: [PolarSeparableFilterType]
  , invariantFeatureFlag          :: Bool
  , centerLength                  :: Int
  } deriving (Show)

options :: [OptDescr Flag]
options =
  [ Option ['i'] ["inputfile"] (ReqArg InputFile "FILE") "Image path list file."
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
      ['z']
      ["OriginModelName"]
      (ReqArg OriginModelName "NAME")
      "SVM model name for origin predictor."
  , Option ['z'] ["GMMFile"] (ReqArg GMMFile "FILE") "GMM data file."
  , Option ['z'] ["PCAFile"] (ReqArg PCAFile "FILE") "PCA data file."
  , Option
      ['z']
      ["threshold"]
      (ReqArg (Threshold . readDouble) "DOUBLE")
      "Set the stoppint criteria. It is the percentage that the probability increases. If it is lower than the threshold, then the program stops."
  , Option
      ['z']
      ["numGaussian"]
      (ReqArg (NumGaussian . readInt) "INT")
      "Set the number of Gaussian in GMM."
  , Option
      ['z']
      ["numGMMExample"]
      (ReqArg (NumGMMExample . readInt) "INT")
      "Set the number of examples which are used for GMM training."
  , Option
      ['z']
      ["numPrincipal"]
      (ReqArg (NumPrincipal . readInt) "INT")
      "Set the output dimension of PCA dimensional reduction."
  , Option
      ['z']
      ["imageSize"]
      (ReqArg (ImageSize . readInt) "INT")
      "Set the size of an square image."
  , Option
      ['z']
      ["paramsFileName"]
      (ReqArg ParamsFileName "NAME")
      "Filter parameter file name."
  , Option
      ['z']
      ["OriginPredictorParamsFileName"]
      (ReqArg OriginPredictorParamsFileName "NAME")
      "Origin Predictor Filter parameter file name."
  , Option
      ['z']
      ["patchSize"]
      (ReqArg (PatchSize . readInt) "INT")
      "Set the patchSize of pooling."
  , Option
      ['z']
      ["stride"]
      (ReqArg (Stride . readInt) "INT")
      "Set the stride of pooling."
  , Option
      ['z']
      ["numBin"]
      (ReqArg (NumBin . readInt) "INT")
      "Set the number of bins for histogram pooling."
  , Option
      ['z']
      ["gaussianScale"]
      (ReqArg
         (\x ->
            let go xs [] = [xs]
                go xs (y:ys) =
                  if y == ','
                    then xs : go [] ys
                    else go (y : xs) ys
            in GaussianScale . map (readDouble . reverse) $ go [] x)
         "[Double]")
      "Set the Gaussian scale list."
  , Option ['z'] ["KMeansFile"] (ReqArg KMeansFile "FILE") "KMeans data file."
  , Option
      ['z']
      ["DataFile"]
      (ReqArg DataFile "FILE")
      "Data file, such as convolution result and vlad"
  , Option ['z'] ["fftwPath"] (ReqArg FFTWWisdomFilePath "FILE") ""
  , Option
      ['z']
      ["logpolarFlag"]
      (NoArg LogpolarFlag)
      "True: use logpolar coordinates; otherwise false."
  , Option
      ['z']
      ["NumGrid"]
      (ReqArg (NumGrid . readInt) "INT")
      "Set the number of grid."
  , Option ['z'] ["Radius"] (ReqArg (Radius . readDouble) "DOUBLE") ""
  , Option ['l'] ["labelFile"] (ReqArg LabelFile "FILE") "Label list file."
  , Option
      ['z']
      ["NumScatteringLayer"]
      (ReqArg (NumScatteringLayer . readInt) "INT")
      "Set the number of scattering layer."
  , Option
      ['z']
      ["FourierMellin"]
      (NoArg FourierMellin)
      "Use FourierMellin Filters"
  , Option
      ['z']
      ["GaussianPinwheel"]
      (NoArg GaussianPinwheel)
      "Use Gaussian-windowed GaussianPinwheel Filters"
  , Option ['z'] ["PinwheelFan"] (NoArg PinwheelFan) "Use PinwheelFan Filters"
  , Option
      ['z']
      ["PinwheelRing"]
      (NoArg PinwheelRing)
      "Use PinwheelRing Filters"
  , Option
      ['z']
      ["PinwheelBlob"]
      (NoArg PinwheelBlob)
      "Use PinwheelBlob Filters"
  , Option
      ['z']
      ["InvariantFeature"]
      (NoArg InvariantFeature)
      "Use invariant features"
  ,  Option
       ['z']
       ["CenterLength"]
       (ReqArg (CenterLength . readInt) "INT")
       "Set the length of center part which is consider as a object."
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
  where
    defaultFlag =
      Params
      { inputFile = ""
      , c = 1.0
      , numThread = 1
      , modelName = "model"
      , originModelName = "origin_model"
      , findC = False
      , batchSize = 1
      , gmmFile = "gmm.dat"
      , pcaFile = "pca.dat"
      , threshold = -15
      , numGaussian = 1
      , numGMMExample = 1
      , numPrincipal = 1
      , imageSize = 0
      , paramsFileName = "params.dat"
      , originPredictorParamsFileName = "originPredictorParams.dat"
      , patchSize = 0
      , stride = 1
      , numBin = 1
      , gaussianScale = [1]
      , kmeansFile = "kmeans.dat"
      , dataFile = "data.dat"
      , fftwWisdomPath = "fftwWisdom.dat"
      , logpolarFlag = False
      , numGrid = 1
      , radius = 1
      , labelFile = ""
      , numScatteringLayer = 1
      , filterType = []
      , invariantFeatureFlag = False
      , centerLength = 0
      }
    go [] params = params
    go (x:xs) params =
      case x of
        InputFile str -> go xs (params {inputFile = str})
        Thread n -> go xs (params {numThread = n})
        C v -> go xs (params {c = v})
        ModelName str -> go xs (params {modelName = str})
        OriginModelName str -> go xs (params {originModelName = str})
        FindC -> go xs (params {findC = True})
        BatchSize x' -> go xs (params {batchSize = x'})
        GMMFile str -> go xs (params {gmmFile = str})
        PCAFile str -> go xs (params {pcaFile = str})
        Threshold v -> go xs (params {threshold = v})
        NumGaussian v -> go xs (params {numGaussian = v})
        NumGMMExample v -> go xs (params {numGMMExample = v})
        NumPrincipal v -> go xs (params {numPrincipal = v})
        ImageSize v -> go xs (params {imageSize = v})
        ParamsFileName v -> go xs (params {paramsFileName = v})
        OriginPredictorParamsFileName v ->
          go xs (params {originPredictorParamsFileName = v})
        PatchSize v -> go xs (params {patchSize = v})
        Stride v -> go xs (params {stride = v})
        NumBin v -> go xs (params {numBin = v})
        GaussianScale v -> go xs (params {gaussianScale = v})
        KMeansFile str -> go xs (params {kmeansFile = str})
        DataFile str -> go xs (params {dataFile = str})
        FFTWWisdomFilePath str -> go xs (params {fftwWisdomPath = str})
        LogpolarFlag -> go xs (params {logpolarFlag = True})
        NumGrid v -> go xs (params {numGrid = v})
        Radius v -> go xs (params {radius = v})
        LabelFile str -> go xs (params {labelFile = str})
        NumScatteringLayer v -> go xs (params {numScatteringLayer = v})
        FourierMellin ->
          go
            xs
            (params {filterType = FourierMellinFilterType : filterType params})
        GaussianPinwheel ->
          go
            xs
            (params
             {filterType = GaussianPinwheelFilterType : filterType params})
        PinwheelFan ->
          go
            xs
            (params {filterType = PinwheelFanFilterType : filterType params})
        PinwheelRing ->
          go
            xs
            (params {filterType = PinwheelRingFilterType : filterType params})
        PinwheelBlob ->
          go
            xs
            (params {filterType = PinwheelBlobFilterType : filterType params})
        InvariantFeature -> go xs (params {invariantFeatureFlag = True})
        CenterLength v -> go xs (params {centerLength = v})


parseArgs :: [String] -> IO Params
parseArgs args = do
  flags <- compilerOpts args
  return $ parseFlag flags
