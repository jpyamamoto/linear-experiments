module Main where

import Data.Array.Destination (DArray)
import qualified Data.Array.Destination as DArray
import Data.Array.Polarized
import qualified Data.Array.Polarized.Pull as Pull
import Data.Array.Polarized.Push (Array (..))
import qualified Data.Array.Polarized.Push as Push
import qualified Data.Vector as Vector

main :: IO ()
main = do
  print (Vector.toList (allocRev (transfer (Pull.fromVector (Vector.fromList ([1, 2, 3, 4, 5] :: [Int]))))))
  print (Vector.toList (mergesort (Vector.fromList ([5, 2, 7, 3, 6] :: [Int]))))

alloc :: Push.Array a %1 -> [a]
alloc (Array w) = w singletonWriter
 where
  singletonWriter :: a -> [a]
  singletonWriter a = [a]

-- Versión con alojamientos intermedios al utilizar `transfer` en `merge`.
mergesort :: (Ord a) => Vector.Vector a -> Vector.Vector a
mergesort v = Push.alloc (transfer (mergesort' (Pull.fromVector v)))

merge :: (Ord a) => Vector.Vector a -> Vector.Vector a -> Vector.Vector a
merge v1 v2 = Push.alloc (transfer (merge' (Pull.fromVector v1) (Pull.fromVector v2)))

-- Versión sin alojamientos intermedios al NO utilizar `transfer` en `merge`.
mergesort' :: (Ord a) => Pull.Array a %1 -> Pull.Array a
mergesort' arr =
  Pull.findLength arr
    & \(n, arr') ->
      if n <= 1
        then arr'
        else halve arr' & \(left, right) -> merge' (mergesort' left) (mergesort' right)

merge' :: (Ord a) => Pull.Array a %1 -> Pull.Array a %1 -> Pull.Array a
merge' arr1 arr2 =
  Pull.findLength arr1
    & \(n, arr1') ->
      if n == 0
        then Pull.append arr1' arr2
        else
          Pull.findLength arr2
            & \(m, arr2') ->
              if m == 0
                then Pull.append arr1' arr2'
                else
                  (Pull.index arr1' 0, Pull.index arr2' 0)
                    & \((x, arr1''), (y, arr2'')) ->
                      if x <= y
                        then
                          Pull.split 1 arr1'' & \(h, t) ->
                            Pull.append h (merge' t arr2'')
                        else
                          Pull.split 1 arr2'' & \(h, t) ->
                            Pull.append h (merge' arr1'' t)

halve :: Pull.Array a %1 -> (Pull.Array a, Pull.Array a)
halve arr =
  Pull.findLength arr
    & \(n, arr') ->
      (move n, arr')
        & \(Ur n', arr'') -> Pull.split (n' `div` 2) arr''

-- Objeto de escritura.

data RevArrayWriter a where
  RevArrayWriter :: (DArray a %1 -> ()) %1 -> !Int -> RevArrayWriter a

instance Semigroup (RevArrayWriter a) where
  (<>) = addWriters

instance Monoid (RevArrayWriter a) where
  mempty = emptyWriter

addWriters :: RevArrayWriter a %1 -> RevArrayWriter a %1 -> RevArrayWriter a
addWriters (RevArrayWriter k1 n) (RevArrayWriter k2 m) =
  RevArrayWriter
    ( \darr ->
        DArray.split m darr & \(arrLeft, arrRight) -> consume (k2 arrLeft, k1 arrRight)
    )
    (n + m)

emptyWriter :: RevArrayWriter a
emptyWriter = RevArrayWriter DArray.dropEmpty 0

allocRev :: Push.Array a %1 -> Vector.Vector a
allocRev (Push.Array c) = allocArray (c singletonWriter)
 where
  singletonWriter :: a -> RevArrayWriter a
  singletonWriter a = RevArrayWriter (DArray.replicate a) 1

  allocArray :: RevArrayWriter a %1 -> Vector.Vector a
  allocArray (RevArrayWriter writer len) = DArray.alloc len writer
