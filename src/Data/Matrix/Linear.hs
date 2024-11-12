{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Data.Matrix.Linear (
  Matrix,
  alloc,
  allocDefault,
  allocBeside,
  allocFromFunction,
  zero,
  identity,
  size,
  nrows,
  ncols,
  get,
  read,
  set,
  write,
  copyEntry,
  freeze,
  entryWiseOp,
  sumEntries,
  prodEntries,
) where

import Prelude hiding (read, zero)

import qualified Data.Array.Mutable.Linear as Array
import Data.Default
import qualified Data.Functor.Linear as Data
import qualified Data.Matrix as UrMatrix
import Data.Vector ((!))

data Matrix a where
  Matrix :: Int -> Int -> Array.Array a %1 -> Matrix a

alloc :: Int -> Int -> a -> (Matrix a %1 -> Ur b) %1 -> Ur b
alloc h w e f = Array.alloc (w * h) e (f . Matrix h w)

allocDefault :: (Default a) => Int -> Int -> (Matrix a %1 -> Ur b) %1 -> Ur b
allocDefault h w = alloc h w def

allocBeside :: Int -> Int -> a -> Matrix b %1 -> (Matrix a, Matrix b)
allocBeside h w e (Matrix h' w' a) = case Array.allocBeside (h * w) e a of
  (a1, a2) -> (Matrix h w a1, Matrix h' w' a2)

allocFromFunction :: Int -> Int -> ((Int, Int) -> a) -> (Matrix a %1 -> Ur b) %1 -> Ur b
allocFromFunction h w f g = Array.fromList indexedList (g . Matrix h w)
 where
  indexedList = [f (i, j) | i <- [0 .. h], j <- [0 .. w]]

zero :: (Num a) => Int -> Int -> (Matrix a %1 -> Ur b) %1 -> Ur b
zero h w f = alloc h w (fromInteger 0) f

identity :: (Num a) => Int -> (Matrix a %1 -> Ur b) %1 -> Ur b
identity d f = allocFromFunction d d kdelta f
 where
  kdelta (i, j) = if i == j then (fromInteger 1) else (fromInteger 0)

size :: Matrix a %1 -> (Ur (Int, Int), Matrix a)
size (Matrix h w a) = (Ur (h, w), Matrix h w a)

nrows :: Matrix a %1 -> (Ur Int, Matrix a)
nrows (Matrix h w a) = (Ur h, Matrix h w a)

ncols :: Matrix a %1 -> (Ur Int, Matrix a)
ncols (Matrix h w a) = (Ur w, Matrix h w a)

get :: Int -> Int -> Matrix a %1 -> (Ur a, Matrix a)
get y x (Matrix h w a) = case Array.get (w * y + x) a of
  (val, a') -> (val, Matrix h w a')

set :: Int -> Int -> a -> Matrix a %1 -> Matrix a
set y x e (Matrix h w a) = Matrix h w $ Array.set (w * y + x) e a

read :: Matrix a %1 -> Int -> Int -> (Ur a, Matrix a)
read m y x = get y x m

write :: Matrix a %1 -> Int -> Int -> a -> Matrix a
write m y x e = set y x e m

copyEntry :: Int -> Int -> Matrix a %1 -> Matrix a %1 -> (Matrix a, Matrix a)
copyEntry y x orig dest = case get y x orig of
  (Ur v, orig') -> (orig', set y x v dest)

freeze :: Matrix a %1 -> Ur (UrMatrix.Matrix a)
freeze (Matrix h w a) = case Array.freeze a of
  (Ur a') -> Ur $ UrMatrix.matrix h w (\(i, j) -> a' ! ((i - 1) * w + (j - 1)))

map :: (a -> b) -> Matrix a %1 -> Matrix b
map f (Matrix h w a) = Matrix h w $ Array.map f a

entryWiseOp :: (a -> b -> c) -> Matrix a %1 -> Matrix b %1 -> Matrix c %1 -> (Matrix a, Matrix b, Matrix c)
entryWiseOp op (Matrix h1 w1 ma) (Matrix h2 w2 mb) (Matrix h3 w3 mc) =
  arrayOp (h3 * w3) op ma mb mc
    & \(aa, ab, ac) -> (Matrix h1 w1 aa, Matrix h2 w2 ab, Matrix h3 w3 ac)
 where
  arrayOp ::
    Int ->
    (a -> b -> c) ->
    Array.Array a %1 ->
    Array.Array b %1 ->
    Array.Array c %1 ->
    (Array.Array a, Array.Array b, Array.Array c)
  arrayOp i f a1 a2 a3
    | i < 0 = (a1, a2, a3)
    | otherwise = case Array.get i a1 of
        (Ur u, a1') -> case Array.get i a2 of
          (Ur v, a2') -> arrayOp (i - 1) f a1' a2' (Array.set i (f u v) a3)

sumEntries :: (Num a) => Matrix a %1 -> Matrix a %1 -> Matrix a %1 -> (Matrix a, Matrix a, Matrix a)
sumEntries = entryWiseOp add
 where
  add a = forget ((forget (+)) a)

prodEntries :: (Num a) => Matrix a %1 -> Matrix a %1 -> Matrix a %1 -> (Matrix a, Matrix a, Matrix a)
prodEntries = entryWiseOp prod
 where
  prod a = forget ((forget (*)) a)

instance Data.Functor Matrix where
  fmap f m = Data.Matrix.Linear.map (forget f) m

instance Consumable (Matrix a) where
  consume :: Matrix a %1 -> ()
  consume (Matrix _ _ a) = consume a

instance Dupable (Matrix a) where
  dup2 :: Matrix a %1 -> (Matrix a, Matrix a)
  dup2 (Matrix h w a) = case dup2 a of
    (a1, a2) -> (Matrix h w a1, Matrix h w a2)
