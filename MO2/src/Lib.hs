{-# LANGUAGE MultiWayIf #-}

module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = print $ goldenSection 0.0 10.0 0.0001

phi     = 0.61803398875
phi1    = 0.38196601125
func    = \x -> x * x

type LeftPoint                  = Double
type RightPoint                 = Double
type Epsilon                    = Double
type GoldenSectionLeftPoint     = Double
type GoldenSectionRightPoint    = Double
type FuncLeftValue              = Double
type FuncRightValue             = Double

goldenSection :: LeftPoint -> RightPoint -> Epsilon -> Double
goldenSection a b eps =
    recursivePart a b eps u1 u2 f1 f2
    where length    = b - a
          u1        = a + phi  * length
          u2        = a + phi1 * length
          f1        = func u1
          f2        = func u2

recursivePart :: LeftPoint                  ->
                 RightPoint                 ->
                 Epsilon                    ->
                 GoldenSectionLeftPoint     ->
                 GoldenSectionRightPoint    ->
                 FuncLeftValue              ->
                 FuncRightValue             ->
                 Double
recursivePart a b eps u1 u2 f1 f2 =
    if | (b - a) < eps  ->  (a + b) / 2
       | f1 <  f2       ->  recursivePart a   u2  eps rightOnly   u1          rightOnlyValue  f1
       | f1 >  f2       ->  recursivePart u1  b   eps u2          leftOnly    f2              leftOnlyValue
       | f1 == f2       ->  recursivePart u1  u2  eps leftBoth    rightBoth   leftBothValue   rightBothValue
    where leftOnly          = u1 + phi  * (b - u1)
          leftOnlyValue     = func leftOnly
          rightOnly         = a  + phi1 * (u2 - a)
          rightOnlyValue    = func rightOnly
          leftBoth          = u1 + phi1 * (u2 - u1)
          leftBothValue     = func leftBoth
          rightBoth         = u1 + phi  * (u2 - u1)
          rightBothValue    = func rightBoth
