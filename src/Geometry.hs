module Geometry where 

    import Codec.Picture

    data Point = Point { x :: Float
                        , y :: Float
                        , z :: Float 
                        } deriving (Show, Eq, Ord)

    data Circle = Circle { r :: Float
                         , p :: Point
                         , c :: PixelRGB8
                         } deriving Show
                 
    type World = [Circle]

    padd  :: Point -> Point -> Point
    (Point i j k) `padd` (Point l m n) = Point (i+l) (j+m) (k+n)  
    
    psub  :: Point -> Point -> Point
    p1 `psub` p2 = p1 `padd` (p2 `pmul` (-1))
          
    pmul :: Point -> Float -> Point  
    (Point i j k) `pmul` m = Point (i*m) (j*m) (k*m)  
      
    pdot :: Point -> Point -> Float
    (Point i j k) `pdot` (Point l m n) = i*l + j*m + k*n  

    normalize :: Point -> Point 
    normalize (Point x y z) = Point { x = x / l, y = y / l, z = z / l } 
        where l = sqrt ( x ** 2 + y ** 2 + z ** 2)

    world :: World
    world = [
        Circle {r = 5, p = Point { x = -1, y = -1, z = -10 }, c = PixelRGB8 128 20 20}
        , Circle {r = 7, p = Point { x = 5, y = 5, z = -17 }, c = PixelRGB8 20 128 20}
        , Circle {r = 3, p = Point { x = -5, y = 3, z = -9 }, c = PixelRGB8 20 20 128}
        ]

    pointCompute :: Circle -> Point -> Point -> Float -> Float -> Float -> Maybe Float
    pointCompute c o d tca d2 r2 = if t0 < 0 && t1 < 0 then Nothing else Just ret
        where thc = sqrt(r2 - d2)
              t0 = tca - thc
              t1 = tca + thc
              ret = if t0 < 0 then t1 else t0

    rayIntersect :: Circle -> Point -> Point -> Maybe Float
    rayIntersect c o d = 
        if d2 > r2 then 
            Nothing else pointCompute c o d tca d2 r2

        where l = (p c) `psub` o
              tca = l `pdot` d
              d2 = l `pdot` l - tca * tca
              r2 = (r c) * (r c)
