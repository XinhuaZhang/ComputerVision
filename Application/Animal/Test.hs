import           Codec.Picture

main = do
  x <- readImage "/home/xinhua/Dataset/Animal/bird/bird1.tif"
  case x of
    Left err -> error $ "err:" ++ err
    Right img ->
      case img of
        ImageY8 _ -> error "ImageY8"
        ImageY16 _ -> error "ImageY16"
        ImageYF _ -> error "ImageYF"
        ImageYA8 _ -> error "ImageYA8"
        ImageYA16 _ -> error "ImageYA16"
        ImageRGB8 _ -> error "ImageRGB8"
        ImageRGB16 _ -> error "ImageRGB16"
        ImageRGBF _ -> error "ImageRGBF"
        ImageRGBA8 _ -> error "ImageRGBA8"
        ImageRGBA16 _ -> error "ImageRGBA16"
        ImageYCbCr8 _ -> error "ImageYCbCr8"
        ImageCMYK8 _ -> error "ImageCMYK8"
        ImageCMYK16 _ -> error "ImageCMYK16"
