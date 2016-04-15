{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Aeson.Versions.Version where

import GHC.TypeLits

import Text.Read hiding (String)
import Text.ParserCombinators.ReadP

import Data.Proxy

import Data.Promotion.Prelude
import Data.Singletons

-- | datatype to use for versions across the board.
data Version a b = MkVersion { majorVersion :: a
                             , minorVersion :: b
                             } deriving (Eq, Ord)


type V1 = MkVersion 1 0
type V2 = MkVersion 2 0
type V3 = MkVersion 3 0
type V4 = MkVersion 4 0
type V5 = MkVersion 5 0

v1, v2, v3, v4, v5 :: Version Integer Integer
v1 = MkVersion 1 0
v2 = MkVersion 2 0
v3 = MkVersion 3 0
v4 = MkVersion 4 0
v5 = MkVersion 5 0

pv1 :: Proxy V1
pv1 = Proxy

pv2 :: Proxy V2
pv2 = Proxy

pv3 :: Proxy V3
pv3 = Proxy

pv4 :: Proxy V4
pv4 = Proxy

pv5 :: Proxy V5
pv5 = Proxy


type family v1 <==? v2 where
  MkVersion a b <==? MkVersion a c = b <=? c
  MkVersion a b <==? MkVersion c d = a <=? c


type MaxV v1 v2 = If (v1 <==? v2) v2 v1

data MaxV' :: v2 -> TyFun (Version Nat Nat) (Version Nat Nat) -> * where
  MaxV' :: MaxV' v1 v2

type instance Apply (MaxV' v1) v2 = MaxV v1 v2


type family MaxVersion vs where
  MaxVersion '[] = Nothing
  MaxVersion (x ': xs) = Just (Maybe_ x (MaxV' x) (MaxVersion xs))

versionParser :: ReadP (Version Integer Integer)
versionParser = do
  majorS <- munch1 (`elem` ['0'..'9'])
  char '.'
  minorS <- munch1 (`elem` ['0'..'9'])
  return $ MkVersion (read majorS) (read minorS)

instance Read (Version Integer Integer) where
    readPrec = lift versionParser

instance Show (Version Integer Integer) where
    show (MkVersion maj min) = show maj ++ "." ++ show min


-- this next section is boilerplate to get versions as types
-- into versions as values. This is useful so that we can write
-- normal `ToJSON` instances for each version by using `Tagged`

-- | Singleton for storing the Version values associated with the Version types
newtype SVersion (v :: Version Nat Nat) = SVersion { unversion :: Version Integer Integer }

-- | General way to get a singleton version out of the type. Based of the type
-- classes in GHC.TypeLits
class KnownVersion (v :: Version Nat Nat) where
  versionSing :: SVersion v

versionVal :: KnownVersion v => proxy v -> Version Integer Integer
versionVal (_ :: proxy v) = unversion (versionSing :: SVersion v)

instance (KnownNat major, KnownNat minor) => KnownVersion (MkVersion major minor) where
    versionSing = SVersion $ MkVersion (natVal (Proxy :: Proxy major)) (natVal (Proxy :: Proxy minor))
