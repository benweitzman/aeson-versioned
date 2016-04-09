{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Aeson.Versions.Internal where

--------------------------------------------------------------------------------

import Data.Aeson.Versions.AesonExtensions
import Data.Aeson.Versions.CatMaybes
import Data.Aeson.Versions.Version

--------------------------------------------------------------------------------

import Control.Applicative

import Data.Aeson
import Data.Aeson.Types

import Data.Functor.Identity

import qualified Data.HashMap.Strict as HM

import Data.Map (Map)
import qualified Data.Map as M

import Data.Proxy

import Data.Tagged

import qualified Data.Text as T

import Data.Traversable

import Prelude hiding (sequence)

-- | Type alias for `FailableToJSON` values
type Serializer a = a -> Maybe Value

-- | Type alias for `FromJSON` values
type Deserializer a = Value -> Parser a

class SerializedVersion a where
  serializers :: Map (Version Integer Integer) (Serializer a)

class DeserializedVersion a where
  deserializers :: Map (Version Integer Integer) (Deserializer a)


serialize :: (CatMaybes f, FunctorToJSON f) => Serializer a -> f a -> Maybe Value
serialize serializer obj = fToJSON <$> catMaybes' (serializer <$> obj)

-- | Serialize the object for all versions into a json object where
-- the keys are versions and the values are version serialized
-- values.
serializeAll :: (CatMaybes f, FunctorToJSON f, SerializedVersion a) => f a -> Value
serializeAll val = Object $ HM.fromList . M.toList
                          $ M.mapKeys (T.pack . show) . M.mapMaybe (flip serialize val)
                          $ serializers

serializeAll' :: (SerializedVersion a) => a -> Value
serializeAll' = serializeAll . Identity

deserialize :: (TraversableFromJSON t) => Deserializer a -> Value -> Maybe (t a)
deserialize deserializer val = flip parseMaybe val $ \x -> do
          fVal <- fParseJSON x
          sequence (fmap deserializer fVal)

-- | Default serialization for anything with a `ToJSON` instance.
instance {-# OVERLAPPABLE #-} ToJSON a => SerializedVersion a where
  serializers = M.fromList [(v1, mToJSON)]

-- | Default deserialization for anything with a `FromJSON` instance
instance {-# OVERLAPPABLE #-} FromJSON a => DeserializedVersion a where
  deserializers = M.fromList [(v1, parseJSON)]


data Void

instance SerializedVersion Void where
    serializers = M.fromList []

instance DeserializedVersion Void where
    deserializers = M.fromList []

-- | Pull down version from type and use it to lookup serializer from map
-- This lets you write normal `ToJSON` instances for `Tagged` versions of the
-- basetype
getSerializer :: (KnownVersion v, FailableToJSON (Tagged v a)) => Proxy v -> (Version Integer Integer, Serializer a)
getSerializer (p :: Proxy v) = (versionVal p, \(obj :: a) -> mToJSON (Tagged obj :: Tagged v a))

-- | Pull down version from type and use it to lookup deserializer from map
-- This lets you write normal `FromJSON` instances for `Tagged` versions of the
-- basetype
getDeserializer :: forall v a. (KnownVersion v, FromJSON (Tagged v a)) => Proxy v -> (Version Integer Integer, Deserializer a)
getDeserializer (p :: Proxy v) = (versionVal p, \value -> unTagged <$> (parseJSON value :: Parser (Tagged v a)))
