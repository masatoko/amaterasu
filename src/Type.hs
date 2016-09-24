module Type where

import qualified Data.Vector.Unboxed as V
import Linear.Affine
import Linear.V2
import Linear.Metric
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Maybe (mapMaybe)

type Pos = Point V2 Double

type Size = V2 Double

type Angle = Double

--

data Segment =
  Seg Pos Pos
  deriving Show

data Ray =
  Ray Pos Pos
  deriving Show

type Triangle = (Pos, Pos, Pos, V2 Double, V2 Double, V2 Double)

type Polygon = [Pos]

data Rectangle =
  Rectangle Pos Size
  deriving Show

--

data FieldOfView = Fov Pos (V.Vector Triangle)

data Shape -- for export
  = Point Pos
  | Rect Pos (V2 Double)
  deriving Show

----- modules

cross :: Num a => Point V2 a -> Point V2 a -> a
cross (P (V2 ax ay)) (P (V2 bx by)) = ax * by - ay * bx

isIntersectSeg :: Segment -> Segment -> Bool
isIntersectSeg (Seg a1 a2) (Seg b1 b2) =
  condA && condB
  where
    a1a2 = a2 - a1
    a1b1 = b1 - a1
    a1b2 = b2 - a1
    condA = (a1a2 `cross` a1b1) * (a1a2 `cross` a1b2) <= 0
    --
    b1b2 = b2 - b1
    b1a1 = a1 - b1
    b1a2 = a2 - b1
    condB = (b1b2 `cross` b1a1) * (b1b2 `cross` b1a2) <= 0

lineIntersection :: (Pos, Pos) -> (Pos, Pos) -> Pos
lineIntersection (a1, a2) (b1, b2) = P $ V2 x y
  where
    P (V2 x1 y1) = a1
    P (V2 x2 y2) = a2
    P (V2 x3 y3) = b1
    P (V2 x4 y4) = b2
    --
    ksi = (y4 - y3) * (x4 - x1) - (x4 - x3) * (y4 - y1)
    delta = (x2 - x1) * (y4 - y3) - (y2 - y1) * (x4 - x3)
    ramda = ksi / delta
    x = x1 + ramda * (x2 - x1)
    y = y1 + ramda * (y2 - y1)

intersectionSS :: Segment -> Segment -> Maybe Pos
intersectionSS (Seg a1 a2) (Seg b1 b2)
  | condA && condB = Just $ lineIntersection (a1, a2) (b1, b2)
  | otherwise      = Nothing
  where
    a1a2 = a2 - a1
    a1b1 = b1 - a1
    a1b2 = b2 - a1
    condA = (a1a2 `cross` a1b1) * (a1a2 `cross` a1b2) <= 0

    b1b2 = b2 - b1
    b1a1 = a1 - b1
    b1a2 = a2 - b1
    condB = (b1b2 `cross` b1a1) * (b1b2 `cross` b1a2) <= 0

intersectionRS :: Ray -> Segment -> Maybe Pos
intersectionRS ray@(Ray a1 a2) (Seg b1 b2)
  | dist1          = Just b1
  | dist2          = Just b2
  | condA && condB = Just $ lineIntersection (a1, a2) (b1, b2)
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


isOnRay :: Pos -> Ray -> Bool
isOnRay pos (Ray r0 r1) =
  innerProduct >= 0 && dist < 0.1
  where
    P va = r1 - r0
    P vb = pos - r0
    distA = distance r0 r1
    distB = distance r0 pos
    innerProduct = va `dot` vb
    theta = acos . min 1 $ innerProduct / (distA * distB)
    dist = distB * sin theta

cosOfTwoVec :: V2 Double -> V2 Double -> Double
cosOfTwoVec va vb = theta
  where
    da = distance (pure 0) va
    db = distance (pure 0) vb
    theta = min 1 $ va `dot` vb / (da * db)

mkTri :: Pos -> Pos -> Pos -> Triangle
mkTri a b c = (a, b, c, ab, bc, ca)
  where
    P ab = b - a
    P bc = c - b
    P ca = a - c

withinTri :: Pos -> Triangle -> Bool
withinTri p (a, b, c, ab, bc, ca) =
  h * i >= 0 && i * j >= 0 && j * h >= 0
  where
    h = P ab `cross` (p - b)
    i = P bc `cross` (p - c)
    j = P ca `cross` (p - a)

posWithinRectangle :: Pos -> Rectangle -> Bool
posWithinRectangle (P (V2 x y)) (Rectangle (P (V2 x0 y0)) (V2 w h)) =
  x >= x0 && x <= x1 && y >= y0 && y <= y1
  where
    x1 = x0 + w
    y1 = y0 + h

cutBy :: Segment -> [Segment] -> [Segment]
cutBy a bs
  | null ps   = [a]
  | x == x'   = psToSegs $ sortBy (comparing yof) ps'
  | otherwise = psToSegs $ sortBy (comparing xof) ps'
  where
    (Seg org@(P (V2 x y)) dst@(P (V2 x' y'))) = a
    ps = mapMaybe (intersectionSS a) bs
    ps' = org:ps ++ [dst]
    xof (P (V2 x _)) = x
    yof (P (V2 _ y)) = y
    --
    psToSegs ks = zipWith Seg ks (tail ks)
