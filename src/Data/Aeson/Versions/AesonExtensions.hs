{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Aeson.Versions.AesonExtensions where

import Data.Aeson
import Data.Aeson.Types

import Data.Functor.Identity

import Data.Traversable

import Data.Maybe

-- | A typeclass to represent serializations that can possibly fail
-- for example if a constructor of a sum type is not available for
-- a specific version
class FailableToJSON a where
  mToJSON :: a -> Maybe Value

-- | Anything that has a non failable `ToJSON` instance can be lifted
-- into `FailableToJSON`
instance {-# OVERLAPPABLE #-} ToJSON a => FailableToJSON a where
  mToJSON = Just . toJSON

-- | This is sort of analogous to how `MonadPlus` is related to
-- `Monoid`. Specifies that whatever we put inside the functor,
-- it's still serializable. Instances should just be
-- `fToJSON = toJSON`
class Functor f => FunctorToJSON f where
  fToJSON :: ToJSON a => f a -> Value

-- | The opposite of `FunctorFailableToJSON` (`FromJSON` is
-- already failable). Instances should all be identical, with
-- `fParseJSON` equaling a specific instantiation of `parseJSON`
class Traversable t => TraversableFromJSON t where
  fParseJSON :: FromJSON a => Value -> Parser (t a)

instance FunctorToJSON Identity where
  fToJSON = toJSON

instance TraversableFromJSON Identity where
  fParseJSON = parseJSON

instance FunctorToJSON [] where
  fToJSON = toJSON

instance TraversableFromJSON [] where
  fParseJSON = parseJSON

instance FunctorToJSON Maybe where
  fToJSON = toJSON
