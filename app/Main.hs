{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when, unless, forM_)
import Data.Word (Word8)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Unboxed as U
import Linear.Affine
import Linear.V2
import Linear.V4
import Linear.Vector
import Data.Maybe (mapMaybe)

import SDL (($=))
import qualified SDL

import Amaterasu

main :: IO ()
main = do
  SDL.initializeAll
  win <- SDL.createWindow "Amaterasu" SDL.defaultWindow {SDL.windowInitialSize = V2 600 600}
  SDL.showWindow win

  --
  renderer <- SDL.createRenderer win 0 SDL.defaultRenderer
  SDL.rendererDrawBlendMode renderer $= SDL.BlendAlphaBlend
  SDL.clear renderer
  --

  test renderer

  SDL.destroyWindow win
  SDL.quit

test :: SDL.Renderer -> IO ()
test renderer = do
  SDL.present renderer
  let loop eyeDir eyePos = do
        es <- SDL.pollEvents
        let eyeDir' = modifyEyeDir eyeDir es
            angOrg = fromIntegral eyeDir' / 180 * pi

        SDL.rendererDrawColor renderer $= V4 50 50 50 255
        SDL.clear renderer

        -- 1. Make ObstacleInfo
        let info = makeObstacleInfo polys boundary

        -- 2. Make FieldOfView
        let eye = Eye eyePos angOrg angRange
            (as, its, fov) = makeFieldOfView_ eye info
        -- let fov = makeFieldOfView eye info -- If you need no information

        -- Rendering
        renderEnv renderer polys boundary
        renderResult renderer eye as its fov

        -- 3. Detection - Whether a rectangle is within FieldOfView
        forM_ shapes $ \shape -> do
          SDL.rendererDrawColor renderer $= V4 255 255 0 200
          when (shape `withinFov` fov) $ drawShape renderer shape
        -- ===

        -- instantVisTest
        let ivtPos = P $ V2 105 105
        when (instantVisTest info eyePos ivtPos) $ do
          SDL.rendererDrawColor renderer $= V4 0 0 255 255
          drawPoint renderer ivtPos
        --

        SDL.present renderer
        SDL.delay 30
        let quit = shouldQuit es
        --
        pos <- SDL.getAbsoluteMouseLocation
        unless quit $ loop eyeDir' (fromIntegral <$> pos)
  loop 0 eye0
  where
    eye0 = P $ V2 300 300
    boundary = Rectangle (pure 100) (pure 400)
    polys = [p1, p2, p3, p4, p5]
    p1 = map P [V2 200 250, V2 400 200, V2 450 300, V2 350 350]
    p2 = map P [V2 150 200, V2 200 400]
    p3 = map P [V2 50 300, V2 150 300]
    p4 = map P [V2 50 400, V2 50 500]
    p5 = map P [V2 300 150, V2 350 450]
    angRange = 300 / 180 * pi
    --
    shapes = [Point (P (V2 (5 * x) (5 * y))) | x <- [0..100],y <- [0..100]]

shouldQuit :: [SDL.Event] -> Bool
shouldQuit = elem SDL.QuitEvent . map SDL.eventPayload

modifyEyeDir :: Int -> [SDL.Event] -> Int
modifyEyeDir dir es = (dir + dy) `mod` 360
  where
    dy = sum $ map (work . SDL.eventPayload) es

    work (SDL.MouseWheelEvent dat) =
      fromIntegral $ 10 * dy
      where
        V2 _ dy = SDL.mouseWheelEventPos dat
    work _ = 0

-----

renderResult r (Eye eye _ _) as its fov = do
  -- Angle
  forM_ as $ \a -> do
    let v = angle a ^* 1000
        p1 = eye + P v
    SDL.rendererDrawColor r $= V4 255 255 255 50
    SDL.drawLine r (round <$> eye) (round <$> p1)
  -- Intersections
  SDL.rendererDrawColor r $= V4 255 100 100 255
  mapM_ (drawPoint r) its
  -- FieldOfView
  SDL.rendererDrawColor r $= V4 0 255 0 200
  renderFov r fov

renderEnv :: SDL.Renderer -> [Polygon] -> Rectangle -> IO ()
renderEnv r polys boundary = do
  SDL.rendererDrawColor r $= white
  mapM_ (drawPoint r) $ concat polys
  --
  SDL.rendererDrawColor r $= white
  mapM_ (drawPolygon r) polys
  forM_ (rectToSegments boundary) $ \(Seg a b) ->
    SDL.drawLine r (round <$> a) (round <$> b)
  where
    white = V4 255 255 255 100

renderFov :: SDL.Renderer -> FieldOfView -> IO ()
renderFov r (Fov eye tris) =
  U.mapM_ renderTriangle tris
  where
    renderTriangle (a, b, c, _, _, _) = do
      SDL.drawLine r (f a) (f b)
      SDL.drawLine r (f b) (f c)
      SDL.drawLine r (f c) (f a)
      where
        f = fmap round

drawPoint :: SDL.Renderer -> Pos -> IO ()
drawPoint r pos = mapM_ work ps
  where
    pos' = round <$> pos
    ps = map (pos' +) [P (V2 dx dy) | dx <- [-3,3], dy <- [-3,3]]
    work = SDL.drawLine r pos'

drawPolygon :: SDL.Renderer -> Polygon -> IO ()
drawPolygon r ps@(p0:_) =
  SDL.drawLines r ps'
  where
    ps' = V.fromList $ map (fmap round) $ ps ++ [p0]

drawShape :: SDL.Renderer -> Shape -> IO ()
drawShape r (Point pos) = SDL.drawPoint r $ round <$> pos
drawShape r (Rect pos size) =
  forM_ (rectToSegments $ Rectangle pos size) $ \(Seg a b) ->
    SDL.drawLine r (round <$> a) (round <$> b)

rectToSegments :: Rectangle -> [Segment]
rectToSegments (Rectangle (P (V2 x y)) (V2 w h)) =
  [Seg p0 p1, Seg p1 p2, Seg p2 p3, Seg p3 p0]
  where
    p0 = P $ V2 x y
    p1 = P $ V2 (x + w) y
    p2 = P $ V2 (x + w) (y + h)
    p3 = P $ V2 x (y + h)
