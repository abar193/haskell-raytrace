module Lib
    ( someFunc
    ) where

    import Prelude as P
    import Codec.Picture
    import Data.Tuple
    import Geometry 

    data Camera = Camera { fov :: Float, o :: Point }

    cam = Camera { fov = 90, o = Point{x = 0, y = 0, z = 0} }
    back = PixelRGB8 20 50 50

    pcol :: (PixelRGB8, Maybe Float) -> PixelRGB8
    pcol (_, Nothing) = back
    pcol (circle, _) =  circle

    calc :: Point -> Point -> Circle -> (PixelRGB8, Maybe Float)
    calc p1 p2 ci = (c ci, rayIntersect ci p1 p2)

    nearest :: World -> Point -> Point -> PixelRGB8
    nearest w p1 p2 = pcol (head wt)
        where wt = map (calc p1 p2) w

    pixelRenderer :: World -> Camera -> Int -> Int -> PixelRGB8
    pixelRenderer w cam i j = nearest w (o cam) d
        where rx = (2 * (fromIntegral i + 0.5) / 300 - 1) * tan( (fov cam) / 2.0 ) * 300 / 300
              ry = -(2 * (fromIntegral j + 0.5) / 300.0 - 1) * tan( (fov cam) / 2.0 )
              d = normalize Point { x = rx, y = ry, z = -1}
              c = head w

    imageCreator :: String -> World -> Camera -> IO ()
    imageCreator path w c = writePng path $ generateImage (pixelRenderer w c) 300 300

    someFunc :: IO ()
    someFunc = imageCreator "test.png" world cam
