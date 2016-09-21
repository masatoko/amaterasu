module Amaterasu where

import Data.List (sort, sortBy, find)
import Linear.Affine
import Linear.V2
import Linear.Vector
import Linear.Metric
import Data.Maybe (mapMaybe, catMaybes, fromMaybe)
import Data.Ord (comparing)
import Safe (headMay)

import Type

makeFieldOfView :: Pos -> Angle -> Angle -> [Polygon] -> Rectangle -> FieldOfView
makeFieldOfView eye aOrg aRange polygons boundary =
  let (_,_,fov) = makeFieldOfView' eye aOrg aRange polygons boundary
  in fov

makeFieldOfView' :: Pos -> Angle -> Angle -> [Polygon] -> Rectangle -> ([Angle], [Pos], FieldOfView)
makeFieldOfView' eye aOrg aRange polygons boundary =
  (as, map fst (concat rayIntersections), fov)
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
      where angles = sort $ mapMaybe (withinA . angleTo eye) ps
    raysFromEye =
      map (Ray eye . (eye +) . P . angle) as

    -- Segments
    segs = rectToSegments boundary ++ concatMap polygonToSegments polygons

    --
    rayIntersections :: [[(Pos, Int)]]
    rayIntersections = map findIntersections raysFromEye
      where
        findIntersections ray =
          map reform $ sortBy compE $ mapMaybe work $ zip [0..] segs
          where
            work (idx, seg) =
              case isIntersectRS ray seg of
                Nothing  -> Nothing
                Just pos -> Just (pos, idx, seg)

            compE a b =
              case compare a0 b0 of
                EQ   -> compare a1 b1
                ord0 -> ord0
              where
                (a0, a1) = eval a
                (b0, b1) = eval b

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
  Fov eye $ map (mkTri . snd) . catSegs . catMaybes $ zipWith work ass0 (tail ass0)
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

    mkTri :: Segment -> Triangle
    mkTri (Seg a b) = Tri eye a b
--

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
