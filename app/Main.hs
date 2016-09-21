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
        SDL.rendererDrawColor rnd $= black
        SDL.clear rnd
        putStrLn "."

        let as = makeFieldOfView eye angOrg angRange ps boundary
        print $ map (\a -> round $ a / pi * 180) as
        renderEnv rnd eye ps boundary
        forM_ as $ \a -> do
          let v = angle a ^* 1000
              p1 = eye + P v
          SDL.rendererDrawColor rnd $= V4 255 255 255 50
          SDL.drawLine rnd (round <$> eye) (round <$> p1)

        SDL.present rnd
        SDL.delay 100
        quit <- shouldQuit
        --
        pos <- SDL.getAbsoluteMouseLocation
        unless quit $ loop (fromIntegral <$> pos)
  loop eye0
  where
    eye0 = P $ V2 400 300
    boundary = Rect (pure 0) (V2 600 600)
    ps = [p1]
    p1 = map P [V2 350 50, V2 500 100, V2 500 200, V2 350 250]
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
