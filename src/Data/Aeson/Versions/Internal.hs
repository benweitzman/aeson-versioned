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

import Control.Error

import Data.Aeson
import Data.Aeson.Types

import Data.Functor.Identity

import qualified Data.HashMap.Strict as HM

import Data.Map (Map)
import qualified Data.Map as M

import Data.Proxy

import Data.Singletons
import Data.Promotion.Prelude.Maybe (FromJust)

import Data.Tagged

import qualified Data.Text as T

import Data.Traversable

import GHC.TypeLits
import GHC.Exts

import Prelude hiding (sequence)

 -- | Turn a list of types into a constraint by applying a type function to each of them
type family AllSatisfy (cf :: TyFun k Constraint -> *) (xs :: [k]) :: Constraint where
    AllSatisfy cf '[] = ()
    AllSatisfy cf (x ': xs) = (Apply cf x, AllSatisfy cf xs)

-- | Type alias for `FailableToJSON` values
type Serializer a = a -> Maybe Value

-- | Type alias for `FromJSON` values
type Deserializer a = Value -> Parser a

-- | type synonym-ish thing for asserting that a constraint applies to the tagged version of
-- an object.
type family HasVersion (c :: * -> Constraint) (a :: *) (v :: Version Nat Nat) :: Constraint where
    HasVersion c a v = (c (Tagged v a))

-- | Term definition to help with partially applying HasVersion so we can send it to
-- `AllSatisfy`
data HasVersion' :: c -> a -> TyFun (Version Nat Nat) (Constraint) -> * where
   HasVersion' :: HasVersion' c a v

-- | Application rule
type instance Apply (HasVersion' c a) v = HasVersion c a v

-- | Indicates that a type supports versioned serialization. Instances are required to
-- define a type list of supported versions, which is checked at compile time to make
-- sure that the approriate `ToJSON` instances actually exist.
class (AllSatisfy (HasVersion' FailableToJSON a) (SerializerVersions a)
      ,ToSerializerMap a (SerializerVersions a)) => SerializedVersion a where
  type SerializerVersions a :: [Version Nat Nat]

-- | Singleton class for pulling a type level list of supported versions into a data level map
-- of versions to serializers
class ToSerializerMap o (a :: [Version Nat Nat]) where
    toSerializerMap :: Tagged a (Map (Version Integer Integer) (Serializer o))

-- | Base case
instance ToSerializerMap o '[] where
    toSerializerMap = Tagged M.empty

-- | Induction case
instance (KnownVersion v, ToSerializerMap o vs, FailableToJSON (Tagged v o)) => ToSerializerMap o (v ': vs) where
    toSerializerMap = let (v', s) = getSerializer (Proxy :: Proxy v)
                          Tagged ss :: Tagged vs (Map (Version Integer Integer) (Serializer o)) = toSerializerMap
                      in Tagged $ M.insert v' s ss

-- | Helper function to pull down serializers from type level and unwrap from Tagged
getSerializers :: forall a. (SerializedVersion a) => Map (Version Integer Integer) (Serializer a)
getSerializers  = let Tagged ss :: Tagged (SerializerVersions a) (Map (Version Integer Integer) (Serializer a)) = toSerializerMap
                  in ss

-- | Indicates that a type supports versioned deserialization. Instances are required
-- to define a type list of supported versions, which is checked at compile time to
-- make sure that the appropriate `FromJSON` instances actually exist.
class (AllSatisfy (HasVersion' FromJSON a) (DeserializerVersions a)
      ,ToDeserializerMap a (DeserializerVersions a)) => DeserializedVersion a where
    type DeserializerVersions a :: [Version Nat Nat]

-- | Singleton class for pulling a type level list of supported versions into a data level
-- map of versions to deserializers
class ToDeserializerMap o (a :: [Version Nat Nat]) where
    toDeserializerMap :: Tagged a (Map (Version Integer Integer) (Deserializer o))

-- | Base case
instance ToDeserializerMap o '[] where
    toDeserializerMap = Tagged M.empty

-- | Induction case
instance (KnownVersion v, ToDeserializerMap o vs, FromJSON (Tagged v o)) => ToDeserializerMap o (v ': vs) where
    toDeserializerMap = let (v', d) = getDeserializer (Proxy :: Proxy v)
                            Tagged ds :: Tagged vs (Map (Version Integer Integer) (Deserializer o)) = toDeserializerMap
                        in Tagged $ M.insert v' d ds

-- | Helper function to pull down deserializers from type level and unwrap from Tagged
getDeserializers :: forall a. (DeserializedVersion a) => Map (Version Integer Integer) (Deserializer a)
getDeserializers = let Tagged ss :: Tagged (DeserializerVersions a) (Map (Version Integer Integer) (Deserializer a)) = toDeserializerMap
                   in ss

-- | Run a serializer on a functor containing values.
runSerializer :: (CatMaybes f, FunctorToJSON f) => Serializer a -> f a -> Maybe Value
runSerializer serializer obj = fToJSON <$> catMaybes' (serializer <$> obj)

data SerializationError = SerializerFailed | SerializerNotFound

serialize :: (CatMaybes f, FunctorToJSON f, SerializedVersion a) => Version Integer Integer -> f a -> Either SerializationError Value
serialize v obj = do
  s <- note SerializerNotFound $ M.lookup v getSerializers
  note SerializerFailed $ runSerializer s obj

serialize' :: (SerializedVersion a) => Version Integer Integer -> a -> Either SerializationError Value
serialize' v = serialize v . Identity

-- | Serialize the object for all versions into a json object where
-- the keys are versions and the values are version serialized
-- values. This is useful if you need to serialize a value to a
-- destination with unknown version. Versions which fail to
-- serialize are not included in the map.
serializeAll :: (CatMaybes f, FunctorToJSON f, SerializedVersion a) => f a -> Value
serializeAll val = Object $ HM.fromList . M.toList
                          $ M.mapKeys (T.pack . show) . M.mapMaybe (`runSerializer` val)
                          $ getSerializers

serializeAll' :: (SerializedVersion a) => a -> Value
serializeAll' = serializeAll . Identity

serializeLatest :: forall max f a.
                   (Just max ~ MaxVersion (SerializerVersions a)
                   ,KnownVersion max, FailableToJSON (Tagged max a)
                   ,FunctorToJSON f, CatMaybes f) =>
                   f a -> Maybe Value
serializeLatest val = let (_, s) = getSerializer (Proxy :: Proxy max)
                      in runSerializer s val

serializeLatest' :: forall max a.
                    (Just max ~ MaxVersion (SerializerVersions a)
                    ,KnownVersion max, FailableToJSON (Tagged max a)) =>
                    a -> Maybe Value
serializeLatest' = serializeLatest . Identity

-- | Run a deserializer
runDeserializer :: (TraversableFromJSON t) => Deserializer a -> Value -> Maybe (t a)
runDeserializer deserializer val = flip parseMaybe val $ \x -> do
          fVal <- fParseJSON x
          sequence (fmap deserializer fVal)

data DeserializationError = DeserializerFailed | DeserializerNotFound

deserialize :: (TraversableFromJSON t, DeserializedVersion a) => Version Integer Integer -> Value -> Either DeserializationError (t a)
deserialize v val = do
  d <- note DeserializerNotFound $ M.lookup v getDeserializers
  note DeserializerFailed $ runDeserializer d val

deserialize' :: (DeserializedVersion a) => Version Integer Integer -> Value -> Either DeserializationError a
deserialize' v val = runIdentity <$> deserialize v val

deserializeLatest :: forall max t a.
                   (Just max ~ MaxVersion (SerializerVersions a)
                   ,KnownVersion max, FromJSON (Tagged max a)
                   ,TraversableFromJSON t) =>
                   Value -> Maybe (t a)
deserializeLatest val = let (_, s) = getDeserializer (Proxy :: Proxy max)
                      in runDeserializer s val

deserializeLatest' :: forall max a.
                      (Just max ~ MaxVersion (SerializerVersions a)
                      ,KnownVersion max, FromJSON (Tagged max a)) =>
                      Value -> Maybe a
deserializeLatest' = (runIdentity <$>) . deserializeLatest


-- | Newtype to specify that a type is going to use the default aeson instances
-- for serialization/deserialization. This will become unnecessary if GHC allows
-- overlapping open type families.
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
