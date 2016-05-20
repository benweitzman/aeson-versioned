{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aeson.Versions.MigrationsSpec where

import Data.Aeson
import Data.Aeson.Versions
import Data.Aeson.Versions.Migrations

import Data.Tagged

import Test.Hspec
import Test.Hspec.QuickCheck



data Thing = Thing Int deriving (Show, Eq)

instance ToJSON (Tagged V1 Thing) where
  toJSON (Tagged (Thing i)) = object ["i" .= i]

instance ToJSON (Tagged V2 Thing) where
  toJSON (Tagged (Thing i)) = object ["i" .= i, "extra" .= ("I'm V2" :: String)]

instance SerializedVersion Thing where
  type SerializerVersions Thing = '[V1, V2]

instance Migratable Int Thing where
  regression (Thing i) = i

instance Migratable from a => Migratable (UsingAeson from) a where
  regression a = UsingAeson $ regression a

m :: Migration (UsingAeson Int) Thing
m = Migration (Thing 3)


data SimplePerson = SimplePerson { sName :: String
                                 , sAge :: Int
                                 }

instance ToJSON (Tagged V1 SimplePerson) where
  toJSON (Tagged (SimplePerson name age)) = object [ "name" .= name
                                                   , "age" .= age
                                                   ]

instance ToJSON (Tagged V2 SimplePerson) where
  toJSON (Tagged (SimplePerson name age)) = object [ "name" .= name
                                                   , "age" .= age
                                                   , "extra" .= ("V2" :: String)
                                                   ]

instance SerializedVersion SimplePerson where
  type SerializerVersions SimplePerson = [V1, V2]

data ComplexPerson = ComplexPerson { cName :: String
                                   , cAge :: Int
                                   , cFollowers :: Int
                                   }

instance ToJSON (Tagged V1 ComplexPerson) where
  toJSON (Tagged (ComplexPerson name age followers)) = object [ "name" .= name
                                                              , "age" .= age
                                                              , "followers" .= followers
                                                              ]

instance ToJSON (Tagged V2 ComplexPerson) where
  toJSON (Tagged (ComplexPerson name age followers)) = object [ "name" .= name
                                                              , "age" .= age
                                                              , "followers" .= followers
                                                              , "extra" .= ("V2" :: String)
                                                              ]

instance ToJSON (Tagged V3 ComplexPerson) where
  toJSON (Tagged (ComplexPerson name age followers)) = object [ "name" .= name
                                                              , "age" .= age
                                                              , "followers" .= followers
                                                              , "extra" .= ("V3" :: String)
                                                              ]

instance SerializedVersion ComplexPerson where
  type SerializerVersions ComplexPerson = [V1, V2, V3]

instance Migratable SimplePerson ComplexPerson where
  regression (ComplexPerson name age _) = SimplePerson name age

m2 :: Migration SimplePerson ComplexPerson
m2 = Migration (ComplexPerson "ben" 25 0)

spec :: Spec
spec = do
  describe "migrations" $ do
    it "migrates from a simple type to a complex type" $ do
      let Right encodedV1 = serialize' v1 m
          Right encodedV2 = serialize' v2 m
      encodedV1 `shouldBe` toJSON (3 :: Int)
      encodedV2 `shouldBe` object [ "i" .= (3 :: Int)
                                  , "extra" .= ("I'm V2" :: String)
                                  ]
    it "migrates from a complex type to a complex type" $ do
      let Right encodedV1 = serialize' v1 m2
          Right encodedV2 = serialize' v2 m2
          Right encodedV3 = serialize' v3 m2
      encodedV1 `shouldBe` object [ "name" .= ("ben" :: String)
                                  , "age" .= (25 :: Int)
                                  ]
      encodedV2 `shouldBe` object [ "name" .= ("ben" :: String)
                                  , "age" .= (25 :: Int)
                                  , "extra" .= ("V2" :: String)
                                  ]
      encodedV3 `shouldBe` object [ "name" .= ("ben" :: String)
                                  , "age" .= (25 :: Int)
                                  , "followers" .= (0 :: Int)
                                  , "extra" .= ("V3" :: String)
                                  ]
