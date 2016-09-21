{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless)
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
  -- makeFieldOfView eye ps boundary
  SDL.present rnd
  let loop = do
        SDL.rendererDrawColor rnd $= black
        SDL.clear rnd
        putStrLn "."

        renderEnv rnd eye ps boundary

        SDL.present rnd
        SDL.delay 100
        quit <- shouldQuit
        unless quit loop
  loop
  where
    eye = P $ V2 400 300
    boundary = Rect (pure 0) (V2 600 600)
    ps = [p1]
    p1 = map P [V2 350 50, V2 500 100, V2 500 200, V2 350 250]

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
