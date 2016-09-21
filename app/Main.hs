module Main where

import Linear.Affine
import Linear.V2

import Amaterasu
import Type

main :: IO ()
main = test

test :: IO ()
test =
  -- makeFieldOfView eye ps boundary
  return ()
  where
    eye = P $ V2 400 300
    boundary = Rect (pure 0) (V2 800 600)
    ps = [p1]
    p1 = map P [V2 350 50, V2 500 100, V2 500 200, V2 350 250]
