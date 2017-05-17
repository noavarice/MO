module Lib
    ( someFunc
    ) where

import Text.Printf

someFunc :: IO ()
someFunc =
        printf "%f, %f\n" (fst point) (snd point)
        where
            point = fastestDescent (103.0, 34.0) 0.000001 10000

f x y = (x - 4) ^ 2 + (y + 7) ^ 2 - 4

f_der_x x y = 2 * x - 8

f_der_y x y = 2 * y + 14

gradient x y = (f_der_x x y, f_der_y x y)

measure (x1, y1) (x2, y2) = sqrt $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2

step = 0.001

fastestDescent :: (Double, Double) -> Double -> Integer -> (Double, Double)
fastestDescent startPoint eps iterCount
        | iterCount == 0                        = startPoint
        | measure startPoint nextPoint < eps    = nextPoint
        | otherwise                             = fastestDescent nextPoint eps (iterCount - 1)
        where
            x = fst startPoint
            y = snd startPoint
            gradientValue= gradient x y
            nextPoint = (x - step * fst gradientValue, y - step * snd gradientValue)
