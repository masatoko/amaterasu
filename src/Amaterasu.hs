module Amaterasu where

import Data.List (sort)
import Linear.Affine
import Linear.V2
import Linear.Vector
import Data.Maybe (mapMaybe)

import Type

makeFieldOfView :: Pos -> Angle -> Angle -> [Polygon] -> Rectangle -> ([Angle], [Pos]) -- FieldOfView
makeFieldOfView eye aOrg aRange polygons boundary = let
  its = map snd . filter fst $ [(intersectRS ray seg, intersectionRS ray seg) | ray <- raysFromEye, seg <- segs]
  in (as, its)
  where
    aDst = aOrg + aRange
    withinA a
      | work a    = Just a
      | work a'   = Just a'
      | otherwise = Nothing
      where
        a' = a + 2 * pi
        work x = x >= aOrg && x <= aDst

    -- Calculate angles
    ps = rectToSidePoints boundary ++ concat polygons
    as = aOrg : angles ++ [aDst]
      where
        angles = sort $ mapMaybe (withinA . angleTo eye) ps
    raysFromEye =
      map (Ray eye . (eye +) . (^* 1000) . P . angle) as

    -- Segments
    segs = rectToSegments boundary ++ concatMap polygonToSegments polygons

rectToSidePoints :: Rectangle -> [Pos]
rectToSidePoints (Rect (P (V2 x y)) (V2 w h)) =
  [p0, p1, p2, p3]
  where
    p0 = P $ V2 x y
    p1 = P $ V2 (x + w) y
    p2 = P $ V2 (x + w) (y + h)
    p3 = P $ V2 x (y + h)

rectToSegments :: Rectangle -> [Segment]
rectToSegments (Rect (P (V2 x y)) (V2 w h)) =
  [Seg p0 p1, Seg p1 p2, Seg p2 p3, Seg p3 p0]
  where
    p0 = P $ V2 x y
    p1 = P $ V2 (x + w) y
    p2 = P $ V2 (x + w) (y + h)
    p3 = P $ V2 x (y + h)

polygonToSegments :: Polygon -> [Segment]
polygonToSegments ps =
  seg : zipWith Seg ps (tail ps)
  where
    seg = Seg (last ps) (head ps)

angleOf :: RealFloat a => V2 a -> a
angleOf (V2 x y) = atan2 y x

angleTo :: RealFloat a => Point V2 a -> Point V2 a -> a
angleTo a b = angleOf v
  where
    (P v) = b - a
