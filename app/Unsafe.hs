module Main where

import Control.Monad.ST
import Data.Array.IArray
import Data.Array.ST
import GHC.Arr (unsafeFreezeSTArray)

data PingPong = Ping | Pong deriving (Show)

main :: IO ()
main = print example

example :: (PingPong, PingPong)
example = runST $ do
  -- Creation of an array with 10 boolean elements initialized to False
  marr <- newArray (0, 1) Ping :: ST s (STArray s Int PingPong)
  -- Freeze the array
  arr <- unsafeFreezeSTArray marr

  -- `x` is defined as the value in the first position of the array

  let x = arr ! 0 -- Lazy evaluation
  -- let !x = arr ! 0 -- Strict evaluation

  -- Array update: True is set at the first position
  writeArray marr 0 Pong

  -- `y` is defined as the value in the first position of the array
  let y = arr ! 0

  -- Returns `x` and `y`
  return (x, y)
