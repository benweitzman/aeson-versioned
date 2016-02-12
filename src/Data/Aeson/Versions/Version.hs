{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aeson.Versions.Version where

import GHC.TypeLits

import Text.Read hiding (String)
import Text.ParserCombinators.ReadP

import Data.Proxy

-- | datatype to use for versions across the board.
data Version a b = MkVersion { majorVersion :: a
                             , minorVersion :: b
                             } deriving (Eq, Ord)

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
