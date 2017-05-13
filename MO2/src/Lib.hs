{-# LANGUAGE MultiWayIf #-}

module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = print $ goldenSection (-100) 100000 0.0001

phi     = 0.61803398875
func    = \x -> (x - 2) * (x - 2) + 2

goldenSection :: Double -> Double -> Double -> Double
goldenSection a b eps
    |   b - a < eps =   (a + b) / 2
    |   f1 > f2     =   goldenSection u1 b eps
    |   f1 < f2     =   goldenSection a u2 eps
    | otherwise     =   goldenSection u1 u2 eps
    where length    = b - a
          u1        = a + (1 - phi) * length
          u2        = a + phi * length
          f1        = func u1
          f2        = func u2
