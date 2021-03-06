{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Aeson.Versions.Migrations where

import Data.Aeson

import Data.Map (Map)
import qualified Data.Map as M

import Data.Proxy

import Data.Promotion.Prelude

import Data.Singletons
import Data.Singletons.Decide
import Data.Singletons.Prelude.Bool

import Data.Tagged

import Data.Type.List

import GHC.TypeLits

import Data.Aeson.Versions.AesonExtensions
import Data.Aeson.Versions.Internal
import Data.Aeson.Versions.Version

data Migration from to = Migration { getLatest :: to }

class Migratable from to where
  regression :: to -> from

instance Migratable from a => Migratable (UsingAeson from) a where
  regression a = UsingAeson $ regression a

instance Show to => Show (Migration from to) where
  show (Migration latest) = show latest

instance (Migratable from to
         ,SerializedVersion from
         ,MaxVersion (SerializerVersions from) ~ Just maxFrom
         ,If (v <==? maxFrom) (FailableToJSON (Tagged v from)) (FailableToJSON (Tagged v to))
         ,SingI (v <==? maxFrom))
         => FailableToJSON  (Tagged v (Migration from to)) where
  mToJSON (Tagged (Migration latest)) = case STrue %~ test of
    Proved Refl -> mToJSON old
    Disproved _ -> case SFalse %~ test of
      Proved Refl -> mToJSON new
      Disproved _ -> Nothing

    where test :: Sing (v <==? maxFrom)
          test = sing

          old :: Tagged v from
          old = Tagged $ regression latest

          new :: Tagged v to
          new = Tagged latest



instance (SerializedVersion from, SerializedVersion to
         ,MaxVersion (SerializerVersions from) ~ Just maxFrom
         ,MaxVersion (SerializerVersions to) ~ Just maxTo
         ,(maxTo <==? maxFrom) ~ False
         ,AllSatisfy (HasVersion' FailableToJSON (Migration from to)) (SerializerVersions (Migration from to))
         ,ToSerializerMap (Migration from to) (SerializerVersions (Migration from to)))
         => SerializedVersion (Migration from to) where
  type SerializerVersions (Migration from to) = Union (SerializerVersions from) (SerializerVersions to)
