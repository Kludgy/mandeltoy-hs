module Main where

import Data.Complex

main :: IO ()
main = mapM_ (putStrLn . line) ys
  where
    line y = map (\z -> if z < escape && not (isNaN z) then ' ' else '#') [magnitude $ man (x :+ y) | x <- xs]
    man = mandelbrot iters (seed :+ 0)
    iters = 50
    seed = 0.0099
    escape = 2
    bresx = 50
    bresy = 30
    scale = 1
    xpos = -0.5
    ypos = 0
    xs = map (\x -> x*scale + xpos) [x/bresx | x <- [-bresx..bresx]]
    ys = map (\y -> y*scale + ypos) [y/bresy | y <- [-bresy..bresy]]


-- | n Mandelbrot iterations starting with initial value z and constant c.
mandelbrot :: Int -> Complex Double -> Complex Double -> Complex Double
mandelbrot n z c
  | n <= 0 = z
  | otherwise = mandelbrot (pred n) (z * z + c) c
