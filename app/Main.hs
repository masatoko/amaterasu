{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless, forM_)
import Data.Word (Word8)
import qualified Data.Vector.Storable as V
import SDL.Vect

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
  let loop eye = do
        SDL.rendererDrawColor rnd $= V4 50 50 50 255
        SDL.clear rnd

        let (as, its, fov) = makeFieldOfView eye angOrg angRange ps boundary
        renderEnv rnd eye ps boundary
        forM_ as $ \a -> do
          let v = angle a ^* 1000
              p1 = eye + P v
          SDL.rendererDrawColor rnd $= V4 255 255 255 50
          SDL.drawLine rnd (round <$> eye) (round <$> p1)
        SDL.rendererDrawColor rnd $= V4 255 100 100 255
        mapM_ (drawPoint rnd) its
        SDL.rendererDrawColor rnd $= V4 255 255 0 200
        renderFov rnd fov

        SDL.present rnd
        SDL.delay 30
        quit <- shouldQuit
        --
        pos <- SDL.getAbsoluteMouseLocation
        unless quit $ loop (fromIntegral <$> pos)
  loop eye0
  where
    eye0 = P $ V2 400 300
    boundary = Rect (pure 100) (pure 400)
    ps = [p1, p2]
    p1 = map P [V2 350 150, V2 500 200, V2 500 300, V2 350 350]
    p2 = map P [V2 200 200, V2 200 400]
    p3 = map P [V2 50 50]
    --
    angOrg = 30 / 180 * pi
    angRange = 300 / 180 * pi

shouldQuit :: IO Bool
shouldQuit = elem SDL.QuitEvent . map SDL.eventPayload <$> SDL.pollEvents

black :: V4 Word8
black = V4 0 0 0 255

white :: V4 Word8
white = V4 255 255 255 255

yellow :: V4 Word8
yellow = V4 255 255 0 255

-----

renderEnv :: SDL.Renderer -> Pos -> [Polygon] -> Rectangle -> IO ()
renderEnv r pos polys boundary = do
  SDL.rendererDrawColor r $= white
  mapM_ (drawPoint r) $ concat polys
  --
  SDL.rendererDrawColor r $= yellow
  drawPoint r pos
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
