module Lib
    ( someFunc
    ) where
import Prelude as P
import Codec.Picture

data Point = Float Float Float
data Circle = Point Float
data World = Circle

isInside :: Int -> Int -> Bool
isInside x y = if 
        x >= 125 
        && x <= 175 
        && y >= 125 
        && y <= 175 then True 
                    else False

pixelRenderer :: Int -> Int -> PixelRGB8
pixelRenderer x y = 
    if isInside x y then PixelRGB8 128 (fromIntegral x) (fromIntegral y)
                    else PixelRGB8 0 0 0

imageCreator :: String -> IO ()
imageCreator path = writePng path $ generateImage pixelRenderer 300 300

someFunc :: IO ()
someFunc = imageCreator "test.png"
