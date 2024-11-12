{-# LANGUAGE ScopedTypeVariables #-}

module Data.SummedArea (Table, build, getElem, (!), getArea) where

import Data.Functor.Linear (Functor (fmap))
import qualified Data.Matrix as UrMatrix
import Data.Matrix.Linear (Matrix)
import qualified Data.Matrix.Linear as Matrix
import Data.Tuple.Linear (swap)

newtype Table a = Mk {toMatrix :: UrMatrix.Matrix a}

instance (Show a) => Show (Table a) where
  show t = show $ toMatrix t

build :: (Num a) => Matrix a %1 -> (Matrix a, Ur (Table a))
build m =
  newAuxiliary m
    & swap
    & uncurry (copyRow 0)
    & uncurry columnWiseSum
    & \(orig, aux) -> (orig, freeze' $ rowWiseSum aux)
 where
  freeze' :: Matrix.Matrix a %1 -> Ur (Table a)
  freeze' = fmap Mk . Matrix.freeze

getElem :: Int -> Int -> Table a -> a
getElem y x (Mk m) = UrMatrix.getElem (y + 1) (x + 1) m

(!) :: Table a -> (Int, Int) -> a
(!) t (y, x) = getElem y x t

getArea :: (Num a) => (Int, Int) -> (Int, Int) -> Table a -> a
getArea (y, x) (y', x') t =
  let br = t ! (y', x')
      tr = if y > 0 then t ! (y - 1, x') else (fromInteger 0)
      bl = if x > 0 then t ! (y', x - 1) else (fromInteger 0)
      tl = if x > 0 && y > 0 then t ! (y - 1, x - 1) else (fromInteger 0)
   in ((br - tr) - bl) + tl

--- Table build

newAuxiliary :: (Num a) => Matrix a %1 -> (Matrix a, Matrix a)
newAuxiliary m = case Matrix.size m of
  (Ur (h, w), m') -> Matrix.allocBeside h w (fromInteger 0) m'

copyRow :: Int -> Matrix a %1 -> Matrix a %1 -> (Matrix a, Matrix a)
copyRow row orig dest = Matrix.ncols orig & \(Ur cols, orig') -> copyRow' row (cols - 1) orig' dest
 where
  copyRow' :: Int -> Int -> Matrix a %1 -> Matrix a %1 -> (Matrix a, Matrix a)
  copyRow' r c o d
    | c < 0 = (o, d)
    | otherwise = Matrix.copyEntry r c o d & uncurry (copyRow' r (c - 1))

columnWiseSum :: forall a. (Num a) => Matrix a %1 -> Matrix a %1 -> (Matrix a, Matrix a)
columnWiseSum orig dest = columnWiseSum' 1 orig dest
 where
  columnWiseSum' :: Int -> Matrix a %1 -> Matrix a %1 -> (Matrix a, Matrix a)
  columnWiseSum' r o d = case Matrix.nrows o of
    (Ur rows, o') ->
      if r >= rows
        then (o', d)
        else copyAddRow r o' d & uncurry (columnWiseSum' (r + 1))

copyAddRow :: forall a. (Num a) => Int -> Matrix a %1 -> Matrix a %1 -> (Matrix a, Matrix a)
copyAddRow row orig dest = Matrix.ncols orig & \(Ur cols, orig') -> copyAddRow' row (cols - 1) orig' dest
 where
  copyAddRow' :: Int -> Int -> Matrix a %1 -> Matrix a %1 -> (Matrix a, Matrix a)
  copyAddRow' r c o d
    | c < 0 = (o, d)
    | otherwise = copyAddUp r c o d & uncurry (copyAddRow' r (c - 1))

copyAddUp :: (Num a) => Int -> Int -> Matrix a %1 -> Matrix a %1 -> (Matrix a, Matrix a)
copyAddUp y x orig dest = case Matrix.get y x orig of
  (Ur u, orig') -> case Matrix.get (y - 1) x dest of
    (Ur v, dest') -> (orig', Matrix.set y x (u + v) dest')

rowWiseSum :: forall a. (Num a) => Matrix a %1 -> Matrix a
rowWiseSum matrix = rowWiseSum' 0 matrix
 where
  rowWiseSum' :: Int -> Matrix a %1 -> Matrix a
  rowWiseSum' r m = case Matrix.nrows m of
    (Ur rows, m') ->
      if r >= rows
        then m'
        else addCol r m' & rowWiseSum' (r + 1)

addCol :: forall a. (Num a) => Int -> Matrix a %1 -> Matrix a
addCol row matrix = addCol' row 1 matrix
 where
  addCol' :: Int -> Int -> Matrix a %1 -> Matrix a
  addCol' r c m = case Matrix.ncols m of
    (Ur cols, m') ->
      if c >= cols
        then m'
        else addLeft r c m' & addCol' r (c + 1)

addLeft :: (Num a) => Int -> Int -> Matrix a %1 -> Matrix a
addLeft y x m = case Matrix.get y x m of
  (Ur u, m') -> case Matrix.get y (x - 1) m' of
    (Ur v, m'') -> Matrix.set y x (u + v) m''
