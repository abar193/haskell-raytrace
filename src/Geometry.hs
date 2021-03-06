module Geometry where 

    import Codec.Picture

    type Number = Float

    data Point = Point { x :: Number
                        , y :: Number
                        , z :: Number 
                        } deriving (Show, Eq, Ord)

    data Circle = Circle { cr :: Number
                         , cp :: Point
                         , cc :: PixelRGB8
                         } deriving Show
  
    data Light = Light { lp :: Point, li :: Number }
    data World = World { wo :: [Circle], wl :: [Light] }

    padd  :: Point -> Point -> Point
    (Point i j k) `padd` (Point l m n) = Point (i+l) (j+m) (k+n)  
    
    psub  :: Point -> Point -> Point
    p1 `psub` p2 = p1 `padd` (p2 `pmul` (-1))
          
    pmul :: Point -> Number -> Point  
    (Point i j k) `pmul` m = Point (i*m) (j*m) (k*m)  
      
    pdot :: Point -> Point -> Number
    (Point i j k) `pdot` (Point l m n) = i*l + j*m + k*n  

    normalize :: Point -> Point 
    normalize (Point x y z) = Point { x = x / l, y = y / l, z = z / l } 
        where l = sqrt ( x ** 2 + y ** 2 + z ** 2)

    world :: World
    world = World { wo = [Circle {cr = 5, cp = Point { x = -1, y = -1, z = -10 }, cc = PixelRGB8 50 50 128}
        , Circle {cr = 7, cp = Point { x = 5, y = 5, z = -17 }, cc = PixelRGB8 50 128 50}
        , Circle {cr = 3, cp = Point { x = -5, y = 3, z = -9 }, cc = PixelRGB8 128 50 50}
        ], 
        wl = [ Light { lp = Point { x = -7, y = 15, z = 5}, li = 1 }, 
          Light { lp = Point { x = 7, y = -5, z = 5}, li = 0.3 }
        ] 
      }

    pointCompute :: Circle -> Point -> Point -> Number -> Number -> Number -> Maybe Number
    pointCompute c o d tca d2 r2 = if t0 < 0 && t1 < 0 then Nothing else Just ret
        where thc = sqrt(r2 - d2)
              t0 = tca - thc
              t1 = tca + thc
              ret = if t0 < 0 then t1 else t0

    rayIntersect :: Circle -> Point -> Point -> Maybe Number
    rayIntersect c o d = 
        if d2 > r2 then 
            Nothing else pointCompute c o d tca d2 r2

        where l = (cp c) `psub` o
              tca = l `pdot` d
              d2 = l `pdot` l - tca * tca
              r2 = (cr c) * (cr c)

    castRay i s f = (2 * (fromIntegral i + 0.5) / s - 1) * tan( f / 2.0 )