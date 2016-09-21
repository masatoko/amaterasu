module Type where

import Linear.Affine
import Linear.V2

type Pos = Point V2 Double

type Size = V2 Double

type Angle = Double

--

data Segment = Seg Pos Pos

data Ray = Ray Pos Pos

data Triangle = Tri Pos Pos Pos

type Polygon = [Pos]

data Rectangle = Rect Pos Size

--

data FieldOfView = Fov Pos [Triangle]

----- modules

cross :: Num a => Point V2 a -> Point V2 a -> a
cross (P (V2 ax ay)) (P (V2 bx by)) = ax * by - ay * bx

intersectRS :: Ray -> Segment -> Bool
intersectRS (Ray a1 a2) (Seg b1 b2) =
  condA && condB
  where
    a1a2 = a2 - a1
    a1b1 = b1 - a1
    a1b2 = b2 - a1
    crossA1 = a1a2 `cross` a1b1
    crossA2 = a1a2 `cross` a1b2
    condA = crossA1 * crossA2 <= 0
    --
    bb
      | crossA1 < 0 = b2 - b1
      | otherwise  = b1 - b2
    b1a1 = a1 - b1
    condB = bb `cross` b1a1 >= 0

intersectionRS :: Ray -> Segment -> Pos
intersectionRS ray seg = P $ V2 x y
  where
    Ray (P (V2 x1 y1)) (P (V2 x2 y2)) = ray
    Seg (P (V2 x3 y3)) (P (V2 x4 y4)) = seg
    --
    ksi = (y4 - y3) * (x4 - x1) - (x4 - x3) * (y4 - y1)
    delta = (x2 - x1) * (y4 - y3) - (y2 - y1) * (x4 - x3)
    ramda = ksi / delta
    x = x1 + ramda * (x2 - x1)
    y = y1 + ramda * (y2 - y1)
