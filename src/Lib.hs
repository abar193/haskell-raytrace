module Lib
    ( render
    ) where

    import Prelude as P
    import Codec.Picture
    import Codec.Picture
    import Data.Tuple
    import Data.List
    import Geometry 

    data Camera = Camera { fov :: Number, o :: Point, width :: Number, height :: Number }

    cam = Camera { fov = 90, o = Point{x = 0, y = 0, z = 0}, width = 300, height = 300 }
    back = PixelRGB8 20 50 50

    pcol :: [Light] -> (Circle, Maybe Number) -> PixelRGB8
    pcol _ (_, Nothing) = back
    pcol l (circle, Just f)  = cc circle

    calc :: Point -> Point -> Circle -> (Circle, Maybe Number)
    calc p1 p2 ci = (ci, rayIntersect ci p1 p2)

    dim :: PixelRGB8 -> Number -> PixelRGB8
    dim (PixelRGB8 r g b) f = PixelRGB8 (round $ (fromIntegral r) * f) (round $ (fromIntegral g) * f) (round $ (fromIntegral b) * f)

    nfld :: (Circle, Maybe Number) -> (Circle, Maybe Number) -> (Circle, Maybe Number)
    nfld (_, Nothing) b = b
    nfld a (_, Nothing) = a
    nfld a b = if snd a < snd b then a else b

    light :: (Circle, Maybe Number) -> Point -> Point -> Light -> Number
    light (_, Nothing) _ _ _  = 1.0
    light (circle, Just f) p1 p2 l = min 1 $ max 0 (li l * (ldr `pdot` nrm))
        where hit = p1 `padd` (p2 `pmul` f)
              ldr = normalize $ lp l `psub` hit
              nrm = normalize $ hit `psub` (cp circle)

    nearest :: World -> Point -> Point -> PixelRGB8
    nearest w p1 p2 = dim bc lt
        where wt = foldl1 nfld $ map (calc p1 p2) $ wo w
              bc = pcol (wl w) wt
              lt = min 1 $ sum $ map (light wt p1 p2) $ wl w


    pixelRenderer :: World -> Camera -> Int -> Int -> PixelRGB8
    pixelRenderer w cam i j = nearest w (o cam) d
        where rx = (castRay i (width cam) (fov cam)) * width cam / height cam
              ry = -1.0 * (castRay j (height cam) (fov cam))
              d = normalize Point { x = rx, y = ry, z = -1}

    imageCreator :: String -> World -> Camera -> IO ()
    imageCreator path w cam = writePng path $ generateImage (pixelRenderer w cam) (round $ width cam) (round $ height cam)

    render :: IO ()
    render = imageCreator "results/test.png" world cam

    