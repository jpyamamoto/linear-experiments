module Main where

import Data.Functor.Linear (fmap)
import qualified Data.Matrix as UrMatrix
import qualified Data.Matrix.Linear as Matrix
import qualified Data.SummedArea as SummedArea

cont :: (Num a) => Matrix.Matrix a %1 -> Ur (UrMatrix.Matrix a, SummedArea.Table a)
cont = extract . (mapTuple Matrix.freeze) . SummedArea.build
 where
  extract :: (Ur a, Ur b) %1 -> Ur (a, b)
  extract (Ur x, Ur y) = Ur (x, y)
  mapTuple :: (a %1 -> b) -> (a, c) %1 -> (b, c)
  mapTuple f (x, y) = (f x, y)

main :: IO ()
main = do
  let (Ur (a1, a2)) = Matrix.allocFromFunction 20 15 (\(y, x) -> y * 21 + x) cont
  print $ a2
  print $ SummedArea.getArea (3, 7) (9, 14) a2
  print $ SummedArea.getArea (0, 0) (1, 1) a2
  print $ SummedArea.getArea (8, 8) (14, 14) a2
