module Main where

import Data.Array.Destination
import Prelude hiding (replicate)

main :: IO ()
main = do
  print $ alloc 10 (replicateHalves (0 :: Int) 1)
  print $ alloc 10 (populateHalves (replicate 0) (replicate 1))

replicateHalves :: a -> a -> DArray a %1 -> ()
replicateHalves lval rval arr = case size arr of
  (Ur l, arr') ->
    split (l `div` 2) arr' & \case
      (arrLeft, arrRight) ->
        replicate lval arrLeft `lseq` replicate rval arrRight

populateHalves ::
  (DArray Int %1 -> ()) ->
  (DArray Int %1 -> ()) ->
  DArray Int %1 ->
  ()
populateHalves lf rf arr = case size arr of
  (Ur l, arr') ->
    split (l `div` 2) arr' & \case
      (arrLeft, arrRight) -> lf arrLeft `lseq` rf arrRight
