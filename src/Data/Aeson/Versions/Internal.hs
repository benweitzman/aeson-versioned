{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}

module Data.Aeson.Versions.Internal where

--------------------------------------------------------------------------------

import Data.Aeson.Versions.AesonExtensions
import Data.Aeson.Versions.CatMaybes
import Data.Aeson.Versions.Version

--------------------------------------------------------------------------------

import Control.Applicative

import Data.Aeson
import Data.Aeson.Types

import Data.Constraint

import Data.Functor.Identity

import qualified Data.HashMap.Strict as HM

import Data.Map (Map)
import qualified Data.Map as M

import Data.Proxy

import Data.Singletons

import Data.Tagged

import qualified Data.Text as T

import Data.Traversable

import GHC.TypeLits

import Prelude hiding (sequence)

type family All c xs :: Constraint where
    All cf '[] = ()
    All cf (x ': xs) = (Apply cf x, All cf xs)

-- | Type alias for `FailableToJSON` values
type Serializer a = a -> Maybe Value

-- | Type alias for `FromJSON` values
type Deserializer a = Value -> Parser a

type family HasToJSONVersion a (v :: Version Nat Nat) where
    HasToJSONVersion a v = (FailableToJSON (Tagged v a))

data HasToJSONVersion' :: a -> TyFun (Version Nat Nat) (Constraint) -> * where
   HasToJSONVersion' :: HasToJSONVersion' a v

type instance Apply (HasToJSONVersion' v) c = HasToJSONVersion v c


type family HasFromJSONVersion a (v :: Version Nat Nat) where
    HasFromJSONVersion a v = (FromJSON (Tagged v a))

data HasFromJSONVersion' :: a -> TyFun (Version Nat Nat) (Constraint) -> * where
   HasFromJSONVersion' :: HasFromJSONVersion' a v

type instance Apply (HasFromJSONVersion' v) c = HasFromJSONVersion v c


class (All (HasToJSONVersion' a) (SerializerVersions a)) => SerializedVersion a where
  type SerializerVersions a :: [Version Nat Nat]

class ToSerializerMap o (a :: [Version Nat Nat]) where
    toSerializerMap :: Tagged a (Map (Version Integer Integer) (Serializer o))

instance ToSerializerMap o '[] where
    toSerializerMap = Tagged M.empty

instance (KnownVersion v, ToSerializerMap o vs, FailableToJSON (Tagged v o)) => ToSerializerMap o (v ': vs) where
    toSerializerMap = let (v', s) = getSerializer (Proxy :: Proxy v)
                          Tagged ss :: Tagged vs (Map (Version Integer Integer) (Serializer o)) = toSerializerMap
                      in Tagged $ M.insert v' s ss

getSerializers :: forall a. (SerializedVersion a, ToSerializerMap a (SerializerVersions a)) => Map (Version Integer Integer) (Serializer a)
getSerializers  = let Tagged ss :: Tagged (SerializerVersions a) (Map (Version Integer Integer) (Serializer a)) = toSerializerMap
                  in ss

class (All (HasFromJSONVersion' a) (DeserializerVersions a)) => DeserializedVersion a where
    type DeserializerVersions a :: [Version Nat Nat]

class ToDeserializerMap o (a :: [Version Nat Nat]) where
    toDeserializerMap :: Tagged a (Map (Version Integer Integer) (Deserializer o))

instance ToDeserializerMap o '[] where
    toDeserializerMap = Tagged M.empty

instance (KnownVersion v, ToDeserializerMap o vs, FromJSON (Tagged v o)) => ToDeserializerMap o (v ': vs) where
    toDeserializerMap = let (v', d) = getDeserializer (Proxy :: Proxy v)
                            Tagged ds :: Tagged vs (Map (Version Integer Integer) (Deserializer o)) = toDeserializerMap
                        in Tagged $ M.insert v' d ds

getDeserializers :: forall a. (DeserializedVersion a, ToDeserializerMap a (DeserializerVersions a)) => Map (Version Integer Integer) (Deserializer a)
getDeserializers = let Tagged ss :: Tagged (DeserializerVersions a) (Map (Version Integer Integer) (Deserializer a)) = toDeserializerMap
                   in ss


serialize :: (CatMaybes f, FunctorToJSON f) => Serializer a -> f a -> Maybe Value
serialize serializer obj = fToJSON <$> catMaybes' (serializer <$> obj)

-- | Serialize the object for all versions into a json object where
-- the keys are versions and the values are version serialized
-- values.
serializeAll :: (CatMaybes f, FunctorToJSON f, SerializedVersion a, ToSerializerMap a (SerializerVersions a)) => f a -> Value
serializeAll val = Object $ HM.fromList . M.toList
                          $ M.mapKeys (T.pack . show) . M.mapMaybe (flip serialize val)
                          $ getSerializers

serializeAll' :: (SerializedVersion a, ToSerializerMap a (SerializerVersions a)) => a -> Value
serializeAll' = serializeAll . Identity

deserialize :: (TraversableFromJSON t) => Deserializer a -> Value -> Maybe (t a)
deserialize deserializer val = flip parseMaybe val $ \x -> do
          fVal <- fParseJSON x
          sequence (fmap deserializer fVal)

newtype UsingToJSON a = UsingToJSON a

instance ToJSON a => ToJSON (Tagged v (UsingToJSON a)) where
    toJSON (Tagged (UsingToJSON a)) = toJSON a

newtype UsingFromJSON a = UsingFromJSON a

instance FromJSON a => FromJSON (Tagged v (UsingFromJSON a)) where
    parseJSON val = (Tagged . UsingFromJSON) <$> parseJSON val

-- | Default serialization for anything with a `ToJSON` instance.
instance {-# OVERLAPPABLE #-} ToJSON a => SerializedVersion (UsingToJSON a) where
    type SerializerVersions (UsingToJSON a) = '[V1]

-- | Default deserialization for anything with a `FromJSON` instance
instance {-# OVERLAPPABLE #-} FromJSON a => DeserializedVersion (UsingFromJSON a) where
    type DeserializerVersions (UsingFromJSON a) = '[V1]


data Void

instance SerializedVersion Void where
    type SerializerVersions Void = '[]

instance DeserializedVersion Void where
    type DeserializerVersions Void = '[]

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
