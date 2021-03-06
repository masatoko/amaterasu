module Amaterasu
( ObstacleInfo
, ObstacleOption (..)
, makeObstacleInfo
, makeFieldOfView
, makeFieldOfView_
, withinFov
, withinTri
, instantVisTest
, intersections
, visibleSegments
, Eye (..)
, FieldOfView (..)
, Shape (..)
, makeNoIntersectionSegs
-- , withinFov'
--
, Pos
, Segment (..)
, Triangle (..)
, Polygon (..)
, Rectangle (..)
) where

import Debug.Trace (trace)

import Control.Monad (join)
import Data.List (sort, sortBy, find, nub, tails)
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

data ObstacleOption
  = HasIntersection
  | OnlyPolysAndBoundary
  | IgnoreIntersection
  deriving (Eq, Show)

makeObstacleInfo :: ObstacleOption -> [Polygon] -> Rectangle -> ObstacleInfo
makeObstacleInfo opt polygons boundary = let
  segs = case opt of
    IgnoreIntersection   -> segs0
    OnlyPolysAndBoundary ->
      let work seg = let
            (pCut, ss) = seg `cutBy` segsRect
            in if pCut
                 then filter isInBndComp ss
                 else ss
      in segsRect ++ concatMap work segsPoly
    HasIntersection      ->
      filter isInBndComp $ makeNoIntersectionSegs segs0
  in ObstacleInfo (makeIntersections segs) segs
  where
    segs0 = segsPoly ++ segsRect
    segsRect = rectToSegments boundary
    segsPoly = filter isInBnd . concatMap polygonToSegments $ polygons
      where
        isInBnd (Seg a b) =
          a `posWithinRectangle` boundary || b `posWithinRectangle` boundary

    makeIntersections = nub . concatMap segToPs
      where
        segToPs (Seg a b) = [a,b]

    isInBndComp (Seg a b) =
      a `posWithinRectangle` boundary && b `posWithinRectangle` boundary

data Eye = Eye Pos Angle Angle

makeFieldOfView :: Eye -> ObstacleInfo -> FieldOfView
makeFieldOfView eye info = fov
  where
    (_,_,fov) = makeFieldOfView_ eye info

makeFieldOfView_ :: Eye -> ObstacleInfo -> ([Angle], [Pos], FieldOfView)
makeFieldOfView_ (Eye eye eyeDir eyeRange) (ObstacleInfo ps segs) =
  (as, map fst (concat rayIntersections), fov)
  where
    adjustAng ang
      | ang < 0       = adjustAng $ ang + 2 * pi
      | ang >= 2 * pi = adjustAng $ ang - 2 * pi
      | otherwise     = ang

    aOrg = adjustAng $ eyeDir - eyeRange / 2
    aDst = aOrg + eyeRange

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
    raysFromEye = map work as
      where
        work = Ray eye . (eye +) . P . angle

    rayIntersections :: [[(Pos, Int)]]
    rayIntersections = map findIntersections raysFromEye
      where
        findIntersections ray =
          mapMaybe work $ zip [0..] segs
          where
            work (idx, seg) =
              case intersectionRS ray seg of
                Nothing  -> Nothing
                Just pos -> Just (pos, idx)

    fov = getFov eye rayIntersections

getFov :: Pos -> [[(Pos, Int)]] -> FieldOfView
getFov eye ass0 =
  Fov eye $ V.fromList . toTriangles . catMaybes $ zipWith work ass0 (tail ass0)
  where
    work :: [(Pos, Int)] -> [(Pos, Int)] -> Maybe (Int, Segment)
    work as bs = closest $ mapMaybe make as
      where
        make a@(pa,ia) = (\(pb,_) -> (ia, Seg pa pb)) <$> find ((== ia) . snd) bs
        closest = headMay . sortBy (comparing quad)
          where
            quad (_,p) = eye `qd` center p
            center (Seg a b) = (a + b) ^/ 2

    toTriangles = map (segToTri . snd) . filter (not . isPoint . snd) . catSegs

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

    isPoint :: Segment -> Bool
    isPoint (Seg a b) = a == b

instantVisTest :: ObstacleInfo -> Pos -> Pos -> Bool
instantVisTest (ObstacleInfo _ as) a b =
  not $ any (isIntersectSeg seg) as
  where
    seg = Seg a b

intersections :: ObstacleInfo -> Pos -> Pos -> [Pos]
intersections (ObstacleInfo _ as) a b =
  mapMaybe (intersectionSS seg) as
  where
    seg = Seg a b

visibleSegments :: FieldOfView -> [Segment]
visibleSegments (Fov _ ts) =
  map fromTri $ V.toList ts
  where
    fromTri (_,b,c,_,_,_) = Seg b c

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
