module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = print $ fst $ fastestDescent (1.0, -3.0) 0.001 100

f x y = x ^ 2 + y ^ 2 - 4

f_der_x x y = 2 * x

f_der_y x y = 2 * y

oppositeGrad x y = (-f_der_x x y, -f_der_y x y)

measure (x1, y1) (x2, y2) = sqrt $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2

step = 0.1

fastestDescent :: (Double, Double) -> Double -> Integer -> (Double, Double)
fastestDescent (x, y) eps iterCount
        | iterCount == 0                    = (x, y)
        | measure (x, y) nextPoint < eps    = nextPoint
        | otherwise                         = fastestDescent nextPoint eps $ iterCount - 1
        where
            oppGrad   = oppositeGrad x y
            nextPoint = (x - step * fst oppGrad, y - step * snd oppGrad)
