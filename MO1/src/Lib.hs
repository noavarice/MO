module Lib
    ( binSearch
    ) where

binSearch :: IO ()
binSearch = print $ recursiveSearch 0.0 10.0 0.0001

recursiveSearch :: Double -> Double -> Double -> Double
recursiveSearch a b eps = 
                        if (b - a) < eps
                            then (a + b) / 2
                            else if goLeft
                                then recursiveSearch a          right   eps
                                else recursiveSearch left       b       eps
                        where
                            f           = \a -> (a - 2) * (a - 2)
                            d           = (b - a) / 4
                            left        = (b + a - d) / 2
                            right       = (b + a + d) / 2
                            goLeft      = f left < f right
