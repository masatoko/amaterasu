{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Linear.Affine
-- import Linear.V2
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
  -- makeFieldOfView eye ps boundary
  SDL.present rnd
  SDL.delay 1000
  return ()
  where
    eye = P $ V2 400 300
    boundary = Rect (pure 0) (V2 600 600)
    ps = [p1]
    p1 = map P [V2 350 50, V2 500 100, V2 500 200, V2 350 250]
