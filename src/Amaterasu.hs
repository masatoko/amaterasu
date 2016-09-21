module Amaterasu where

import Data.List (sort)
import Linear.Affine
import Linear.V2
import Data.Maybe (mapMaybe)

import Type

makeFieldOfView :: Pos -> Angle -> Angle -> [Polygon] -> Rectangle -> [Angle] -- FieldOfView
makeFieldOfView eye aOrg aRange polygons boundary = as'
  where
    aDst = aOrg + aRange
    withinA a
      | work a    = Just a
      | work a'   = Just a'
      | otherwise = Nothing
      where
        a' = a + 2 * pi
        work x = x >= aOrg && x <= aDst
    --
    ps = rectToSidePoints boundary ++ concat polygons
    as = sort $ mapMaybe (withinA . angleTo eye) ps
    as' = aOrg : as ++ [aDst]

rectToSidePoints :: Rectangle -> [Pos]
rectToSidePoints (Rect (P (V2 x y)) (V2 w h)) =
  [p0, p1, p2, p3]
  where
    p0 = P $ V2 x y
    p1 = P $ V2 (x + w) y
    p2 = P $ V2 (x + w) (y + h)
    p3 = P $ V2 x (y + h)

angleOf :: RealFloat a => V2 a -> a
angleOf (V2 x y) = atan2 y x

angleTo :: RealFloat a => Point V2 a -> Point V2 a -> a
angleTo a b = angleOf v
  where
    (P v) = b - a
