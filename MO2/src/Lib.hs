{-# LANGUAGE MultiWayIf #-}

module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = print $ goldenSection 0.0 10.0 0.000001

goldenSection :: Double -> Double -> Double -> Double
goldenSection a b eps =
    if | (b - a)   <  eps           -> (a + b) / 2
       | leftValue <  rightValue    -> goldenSection a          rightPoint  eps
       | leftValue >  rightValue    -> goldenSection leftPoint  b           eps
       | leftValue == rightValue    -> goldenSection leftPoint  rightPoint  eps
    where coeff         = 0.61803398875
          length        = b - a
          leftPoint     = a + coeff * length
          rightPoint    = a + (1 - coeff) * length
          f             = \x -> -(x - 1) * (x - 1)
          leftValue     = f leftPoint
          rightValue    = f rightPoint
