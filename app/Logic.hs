{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import qualified Prelude.Linear as L

main :: IO ()
main = print "Compiled"

type Tensor a b = (a, b)

type With a b = forall k. Either (a %1 -> k) (b %1 -> k) %1 -> k

type Plus a b = Either a b

type Par a b = forall k. (a %1 -> k, b %1 -> k) %1 -> k

type One = ()

type Bot = forall k. () %1 -> k

data Top

data Zero

neutralTensor :: Tensor () a %1 -> a
neutralTensor ((), x) = x

neutralPlus :: Plus Zero a %1 -> a
neutralPlus (Left z) = case z of {}
neutralPlus (Right x) = x

neutralWith :: With Top a %1 -> a
neutralWith w = w (Right id)

constructionTensor :: a %1 -> b %1 -> Tensor a b
constructionTensor = (,)

constructionWith :: a -> b -> With a b
constructionWith x y = \p -> case p of
  (Left fa) -> fa x
  (Right fb) -> fb y

constructionPlus :: a -> Plus a b
constructionPlus = Left

constructionPlus' :: b -> Plus a b
constructionPlus' = Right

elimTensor :: (Tensor a b) %1 -> (a %1 -> b %1 -> c) %1 -> c
elimTensor (x, y) f = f x y

elimWith :: With a b %1 -> a
elimWith f = f (Left id)

elimWith' :: With a b %1 -> b
elimWith' f = f (Right id)

elimPlus :: Plus a b %1 -> (a %1 -> c) -> (b %1 -> c) -> c
elimPlus (Left a) fa _fb = fa a
elimPlus (Right b) _fa fb = fb b
