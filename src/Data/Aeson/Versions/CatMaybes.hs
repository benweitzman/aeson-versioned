module Data.Aeson.Versions.CatMaybes where

import Data.Functor.Identity

import Data.Foldable
import Data.Traversable

import Data.Maybe

import Prelude hiding (sequence)

-- | Generalization of catMaybes that allows to remove `Nothings`
-- from singleton types
class Functor f => CatMaybes f where
  catMaybes' :: f (Maybe a) -> Maybe (f a)

instance CatMaybes Maybe where
  catMaybes' = sequence

instance CatMaybes Identity where
  catMaybes' = sequence

instance CatMaybes [] where
  catMaybes' = Just . catMaybes
