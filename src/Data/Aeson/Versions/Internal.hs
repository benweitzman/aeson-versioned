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

type family AllSatisfy cf xs :: Constraint where
    AllSatisfy cf '[] = ()
    AllSatisfy cf (x ': xs) = (Apply cf x, AllSatisfy cf xs)

-- | Type alias for `FailableToJSON` values
type Serializer a = a -> Maybe Value

-- | Type alias for `FromJSON` values
type Deserializer a = Value -> Parser a

type family HasVersion c a (v :: Version Nat Nat) where
    HasVersion c a v = (c (Tagged v a))

data HasVersion' :: c -> a -> TyFun (Version Nat Nat) (Constraint) -> * where
   HasVersion' :: HasVersion' c a v

type instance Apply (HasVersion' c a) v = HasVersion c a v

class (AllSatisfy (HasVersion' FailableToJSON a) (SerializerVersions a)) => SerializedVersion a where
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

class (AllSatisfy (HasVersion' FromJSON a) (DeserializerVersions a)) => DeserializedVersion a where
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

newtype UsingAeson a = UsingAeson a

instance ToJSON a => ToJSON (Tagged v (UsingAeson a)) where
    toJSON (Tagged (UsingAeson a)) = toJSON a

instance FromJSON a => FromJSON (Tagged v (UsingAeson a)) where
    parseJSON val = (Tagged . UsingAeson) <$> parseJSON val

-- | Default serialization for anything with a `ToJSON` instance.
instance {-# OVERLAPPABLE #-} ToJSON a => SerializedVersion (UsingAeson a) where
    type SerializerVersions (UsingAeson a) = '[V1]

-- | Default deserialization for anything with a `FromJSON` instance
instance {-# OVERLAPPABLE #-} FromJSON a => DeserializedVersion (UsingAeson a) where
    type DeserializerVersions (UsingAeson a) = '[V1]


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
