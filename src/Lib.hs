module Lib
    ( someFunc
    ) where

    import Prelude as P
    import Codec.Picture
    import Codec.Picture
    import Data.Tuple
    import Data.List
    import Geometry 

    data Camera = Camera { fov :: Float, o :: Point }

    cam = Camera { fov = 90, o = Point{x = 0, y = 0, z = 0} }
    back = PixelRGB8 20 50 50

    pcol :: [Light] -> (Circle, Maybe Float) -> PixelRGB8
    pcol _ (_, Nothing) = back
    pcol l (circle, Just f)  = cc circle

    calc :: Point -> Point -> Circle -> (Circle, Maybe Float)
    calc p1 p2 ci = (ci, rayIntersect ci p1 p2)

    dim :: PixelRGB8 -> Float -> PixelRGB8
    dim (PixelRGB8 r g b) f = PixelRGB8 (round $ (fromIntegral r) * f) (round $ (fromIntegral g) * f) (round $ (fromIntegral b) * f)

    nfld :: (Circle, Maybe Float) -> (Circle, Maybe Float) -> (Circle, Maybe Float)
    nfld (_, Nothing) b = b
    nfld a (_, Nothing) = a
    nfld a b = if snd a < snd b then a else b

    light :: (Circle, Maybe Float) -> Light -> Point -> Point -> Float
    light (_, Nothing) _ _ _  = 1.0
    light (circle, Just f) l p1 p2 = min 1 $ max 0 (li l * (ldr `pdot` nrm))
        where hit = p1 `padd` (p2 `pmul` f)
              ldr = normalize $ lp l `psub` hit
              nrm = normalize $ hit `psub` (cp circle)

    nearest :: World -> Point -> Point -> PixelRGB8
    nearest w p1 p2 = dim bc $ light wt (head $ wl w) p1 p2
        where wt = foldl1 nfld $ map (calc p1 p2) $ wo w
              bc = pcol (wl w) wt


    pixelRenderer :: World -> Camera -> Int -> Int -> PixelRGB8
    pixelRenderer w cam i j = nearest w (o cam) d
        where rx = (2 * (fromIntegral i + 0.5) / 300 - 1) * tan( (fov cam) / 2.0 ) * 300 / 300
              ry = -(2 * (fromIntegral j + 0.5) / 300.0 - 1) * tan( (fov cam) / 2.0 )
              d = normalize Point { x = rx, y = ry, z = -1}

    imageCreator :: String -> World -> Camera -> IO ()
    imageCreator path w c = writePng path $ generateImage (pixelRenderer w c) 300 300

    someFunc :: IO ()
    someFunc = imageCreator "results/test.png" world cam

    