module Type where

import Debug.Trace (trace)

import Linear.Affine
import Linear.V2
import Linear.Metric

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

intersectRS :: Ray -> Segment -> Maybe Pos
intersectRS ray@(Ray a1 a2) seg@(Seg b1 b2)
  | dist1       = Just b1
  | dist2       = Just b2
  | condA && condB = Just intersection
  | otherwise      = Nothing
  where
    dist1 = b1 `isOnRay` ray
    dist2 = b2 `isOnRay` ray
    --
    a1a2 = a2 - a1
    a1b1 = b1 - a1
    a1b2 = b2 - a1
    crossA1 = a1a2 `cross` a1b1
    crossA2 = a1a2 `cross` a1b2
    condA = crossA1 * crossA2 <= 0
    --
    bb
      | crossA1 < 0 = b2 - b1
      | otherwise   = b1 - b2
    b1a1 = a1 - b1
    condB = bb `cross` b1a1 >= 0

    intersection = P $ V2 x y
      where
        Ray (P (V2 x1 y1)) (P (V2 x2 y2)) = ray
        Seg (P (V2 x3 y3)) (P (V2 x4 y4)) = seg
        --
        ksi = (y4 - y3) * (x4 - x1) - (x4 - x3) * (y4 - y1)
        delta = (x2 - x1) * (y4 - y3) - (y2 - y1) * (x4 - x3)
        ramda = ksi / delta
        x = x1 + ramda * (x2 - x1)
        y = y1 + ramda * (y2 - y1)

isOnRay :: Pos -> Ray -> Bool
isOnRay pos (Ray r0 r1) = dist < 0.1
  where
    P va = r1 - r0
    P vb = pos - r0
    distA = distance r0 r1
    distB = distance r0 pos
    theta = acos . min 1 $ va `dot` vb / (distA * distB)
    dist = distB * sin theta
