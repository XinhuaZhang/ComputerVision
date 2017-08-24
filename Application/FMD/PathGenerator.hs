module Application.FMD.PathGenerator where

import           Data.List       as L
import           System.FilePath
import           Text.Printf

{-# INLINE pathConstructor #-}

pathConstructor :: FilePath -> String -> String -> Int -> FilePath
pathConstructor folderPath nameStr typeStr index =
  folderPath </> nameStr </>
  (nameStr L.++ "_" L.++ typeStr L.++ "_" L.++
   (printf "%03d" index L.++ "_new.jpg"))
   

halfPathGenerator :: FilePath -> Bool -> [(Double, FilePath)]
halfPathGenerator folderPath isTrain =
  [ (1, pathConstructor folderPath "foliage" "final" index)
  | index <-
     if isTrain
       then [1 .. 50]
       else [51 .. 100] ] L.++
  L.concat
    (L.zipWith
       (\textureName label ->
           [ (label, pathConstructor folderPath textureName typeStr index)
           | typeStr <- typeStrs
           , index <-
              if isTrain
                then [1 .. 25]
                else [26 .. 50] ])
       textureNames
       [2 ..])
  where
    textureNames =
      [ "fabric"
      , "glass"
      , "leather"
      , "metal"
      , "paper"
      , "plastic"
      , "stone"
      , "water"
      , "wood"
      ]
    typeStrs = ["moderate", "object"]
