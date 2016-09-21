{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when, unless, forM_)
import Data.Word (Word8)
import qualified Data.Vector.Storable as V
import SDL.Vect
import Data.Maybe (mapMaybe)

import SDL (($=))
import qualified SDL

import Amaterasu
import Type

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
test rnd = do
  SDL.present rnd
  let loop eyeDir eye = do
        es <- SDL.pollEvents
        let eyeDir' = modifyEyeDir eyeDir es

        let angOrg = fromIntegral eyeDir' / 180 * pi
        SDL.rendererDrawColor rnd $= V4 50 50 50 255
        SDL.clear rnd

        let (as, its, fov) = makeFieldOfView' eye angOrg angRange ps boundary

        -- === Rendering
        -- Environment
        renderEnv rnd ps boundary
        -- Angle
        forM_ as $ \a -> do
          let v = angle a ^* 1000
              p1 = eye + P v
          SDL.rendererDrawColor rnd $= V4 255 255 255 50
          SDL.drawLine rnd (round <$> eye) (round <$> p1)
        -- Intersections
        SDL.rendererDrawColor rnd $= V4 255 100 100 255
        mapM_ (drawPoint rnd) its
        -- FieldOfView
        SDL.rendererDrawColor rnd $= V4 255 255 0 200
        renderFov rnd fov
        -- Target
        if target `withinFov` fov
          then SDL.rendererDrawColor rnd $= V4 0 255 255 200
          else SDL.rendererDrawColor rnd $= V4 0 255 255 50
        drawRect rnd target
        -- ===

        SDL.present rnd
        SDL.delay 30
        let quit = shouldQuit es
        --
        pos <- SDL.getAbsoluteMouseLocation
        unless quit $ loop eyeDir' (fromIntegral <$> pos)
  loop 0 eye0
  where
    eye0 = P $ V2 300 300
    boundary = Rect (pure 100) (pure 400)
    ps = [p1, p2]
    p1 = map P [V2 200 250, V2 400 200, V2 450 300, V2 350 350]
    p2 = map P [V2 150 200, V2 200 400]
    angRange = 300 / 180 * pi
    --
    target = Rect (P (V2 450 150)) (pure 30)

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

black :: V4 Word8
black = V4 0 0 0 255

white :: V4 Word8
white = V4 255 255 255 255

yellow :: V4 Word8
yellow = V4 255 255 0 255

-----

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
  mapM_ renderTriangle tris
  where
    renderTriangle (Tri a b c) = do
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
    work =  SDL.drawLine r pos'

drawPolygon :: SDL.Renderer -> Polygon -> IO ()
drawPolygon r ps@(p0:_) =
  SDL.drawLines r ps'
  where
    ps' = V.fromList $ map (fmap round) $ ps ++ [p0]

drawRect :: SDL.Renderer -> Rectangle -> IO ()
drawRect r (Rect (P (V2 x y)) (V2 w h)) =
  SDL.drawLines r $ V.fromList $ map (fmap round) [p0, p1, p2, p3, p0]
  where
    p0 = P $ V2 x y
    p1 = P $ V2 (x + w) y
    p2 = P $ V2 (x + w) (y + h)
    p3 = P $ V2 x (y + h)
