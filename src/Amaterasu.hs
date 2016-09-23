module Amaterasu
( makeObstacleInfo
, makeFieldOfView
, makeFieldOfView_
, withinFov
, FieldOfView (..)
, Shape (..)
-- , withinFov'
--
, Pos
, Segment (..)
, Triangle (..)
, Polygon (..)
, Rectangle (..)
) where

import Data.List (sort, sortBy, find)
import qualified Data.Vector.Unboxed as V
import Linear.Affine
import Linear.V2
import Linear.Vector
import Linear.Metric
import Data.Maybe (mapMaybe, catMaybes, fromMaybe)
import Data.Ord (comparing)
import Safe (headMay)

import Type

data ObstacleInfo =
  ObstacleInfo [Pos] [Segment]
  deriving Show

makeObstacleInfo :: [Polygon] -> Rectangle -> ObstacleInfo
makeObstacleInfo polygons boundary = ObstacleInfo ps segs
  where
    ps = rectToSidePoints boundary ++ concat polygons
    segs = rectToSegments boundary ++ concatMap polygonToSegments polygons

makeFieldOfView :: Pos -> Angle -> Angle -> ObstacleInfo -> FieldOfView
makeFieldOfView eye aOrg aRange info =
  let (_,_,fov) = makeFieldOfView_ eye aOrg aRange info
  in fov

makeFieldOfView_ :: Pos -> Angle -> Angle -> ObstacleInfo -> ([Angle], [Pos], FieldOfView)
makeFieldOfView_ eye aOrg' aRange (ObstacleInfo ps segs) =
  (as, map fst (concat rayIntersections), fov)
  where
    adjustAng ang
      | ang < 0       = adjustAng $ ang + 2 * pi
      | ang >= 2 * pi = adjustAng $ ang - 2 * pi
      | otherwise     = ang
    aOrg = adjustAng aOrg'
    aDst = aOrg + aRange
    withinA a
      | work a'    = Just a'
      | work a''   = Just a''
      | otherwise  = Nothing
      where
        a' = if a < 0 then adjustAng a else a
        a'' = a' + 2 * pi
        work x = x >= aOrg && x <= aDst

    -- Calculate angles
    as = aOrg : angles ++ [aDst]
      where angles = sort $ mapMaybe (withinA . angleTo eye) ps
    raysFromEye =
      map (Ray eye . (eye +) . P . angle) as

    rayIntersections :: [[(Pos, Int)]]
    rayIntersections = map findIntersections raysFromEye
      where
        findIntersections ray =
          map reform $ sortBy (comparing eval) $ mapMaybe work $ zip [0..] segs
          where
            work (idx, seg) =
              case intersectionRS ray seg of
                Nothing  -> Nothing
                Just pos -> Just (pos, idx, seg)

            eval (pos, _, Seg p0 p1) = (e0, e1)
              where
                e0 = eye `qd` pos
                P v1 = pos - eye
                P v2 = if pos == p0 then p1 - p0 else p0 - p1
                e1 = v1 `cosOfTwoVec` v2

            reform (pos, idx, seg) = (pos, idx)

    fov = getFov eye rayIntersections

getFov :: Pos -> [[(Pos, Int)]] -> FieldOfView
getFov eye ass0 =
  Fov eye $ V.fromList . map (segToTri . snd) . catSegs . catMaybes $ zipWith work ass0 (tail ass0)
  where
    work :: [(Pos, Int)] -> [(Pos, Int)] -> Maybe (Int, Segment)
    work as bs =
      headMay $ mapMaybe go as
      where
        go a@(p,i) =
          (\b -> (i, Seg (fst a) (fst b))) <$> find ((== i) . snd) bs

    catSegs :: [(Int, Segment)] -> [(Int, Segment)]
    catSegs []    = []
    catSegs [a]   = [a]
    catSegs (a@(ia,sa):b@(ib,sb):ps)
      | ia == ib  = catSegs $ (ia, connect sa sb) : ps
      | otherwise = a : catSegs (b:ps)

    connect :: Segment -> Segment -> Segment
    connect (Seg a _) (Seg _ b) = Seg a b

    segToTri :: Segment -> Triangle
    segToTri (Seg a b) = mkTri eye a b

--

withinFov :: Shape -> FieldOfView -> Bool
withinFov (Rect pos size) (Fov _ ts) = any work ps
  where
    ps = rectToSidePoints $ Rectangle pos size
    work p = V.any (p `withinTri`) ts

withinFov (Point pos) (Fov _ ts) = V.any (pos `withinTri`) ts

-- withinFov' :: Rectangle -> FieldOfView -> Bool
-- withinFov' rect fov@(Fov _ ts) = condPos || condSeg
--   where
--     condPos = withinFov rect fov
--     condSeg =
--       or [isIntersectSeg a b | a <- ssRect, b <- ssTri]
--       where
--         ssRect = rectToSegments rect
--         ssTri = concatMap fromTri ts
--         fromTri (Tri a b c) = [Seg a b, Seg b c, Seg c a]
--

rectToSidePoints :: Rectangle -> [Pos]
rectToSidePoints (Rectangle (P (V2 x y)) (V2 w h)) =
  [p0, p1, p2, p3]
  where
    p0 = P $ V2 x y
    p1 = P $ V2 (x + w) y
    p2 = P $ V2 (x + w) (y + h)
    p3 = P $ V2 x (y + h)

rectToSegments :: Rectangle -> [Segment]
rectToSegments (Rectangle (P (V2 x y)) (V2 w h)) =
  [Seg p0 p1, Seg p1 p2, Seg p2 p3, Seg p3 p0]
  where
    p0 = P $ V2 x y
    p1 = P $ V2 (x + w) y
    p2 = P $ V2 (x + w) (y + h)
    p3 = P $ V2 x (y + h)

polygonToSegments :: Polygon -> [Segment]
polygonToSegments []    = []
polygonToSegments [_]   = []
polygonToSegments [a,b] = [Seg a b]
polygonToSegments ps    =
  seg : zipWith Seg ps (tail ps)
  where
    seg = Seg (last ps) (head ps)

angleOf :: RealFloat a => V2 a -> a
angleOf (V2 x y) = atan2 y x

angleTo :: RealFloat a => Point V2 a -> Point V2 a -> a
angleTo a b = angleOf v
  where
    (P v) = b - a
