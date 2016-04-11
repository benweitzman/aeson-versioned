{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aeson.VersionsSpec where

import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans

import Data.Aeson
import Data.Aeson.Versions

import Data.Functor.Identity

import qualified Data.Map as M

import qualified Data.HashMap.Strict as HM

import Data.Tagged

import qualified Data.Vector as V

import Test.Hspec
import Test.Hspec.QuickCheck


data Foo = Foo Int String

instance ToJSON (Tagged V1 Foo) where
    toJSON (Tagged (Foo a _)) = object [ "a" .= a ]

instance ToJSON (Tagged V2 Foo) where
    toJSON (Tagged (Foo a b)) = object [ "a" .= a
                                       , "b" .= b
                                       ]

instance SerializedVersion Foo where
    serializers = M.fromList [getSerializer pv1
                             ,getSerializer pv2
                             ]

data Bar = Bar | BarPlus

instance FailableToJSON (Tagged V1 Bar) where
    mToJSON (Tagged Bar) = Just $ object ["type" .= ("bar" :: String)]
    mToJSON (Tagged BarPlus) = Nothing

instance ToJSON (Tagged V2 Bar) where
    toJSON (Tagged Bar) = object ["type" .= ("bar" :: String)]
    toJSON (Tagged BarPlus) = object ["type" .= ("barPlus" :: String)]


instance SerializedVersion Bar where
    serializers = M.fromList [getSerializer pv1
                             ,getSerializer pv2
                             ]

spec :: Spec
spec  = do
  describe "versions" $ do
    it "serializes two versions" $ do
      let val = Identity (Foo 5 "five")
          Just encodedV1 = flip serialize val =<< M.lookup v1 serializers
          Just encodedV2 = flip serialize val =<< M.lookup v2 serializers
      encodedV1 `shouldBe` object ["a" .= (5::Int)]
      encodedV2 `shouldBe` object ["a" .= (5::Int), "b" .= ("five" :: String)]
    it "serializes all versions" $ do
      let encoded = serializeAll (Identity (Foo 5 "five"))
      encoded `shouldBe` object [ "1.0" .= object ["a" .= (5 :: Int)]
                                , "2.0" .= object ["a" .= (5 :: Int)
                                                  ,"b" .= ("five" :: String)
                                                  ]
                                ]
    it "removes unsupported constructors" $ do
      let vals = [Bar, BarPlus]
          Just encodedV1 = flip serialize vals =<< M.lookup v1 serializers
          Just encodedV2 = flip serialize vals =<< M.lookup v2 serializers
          encodedAll = serializeAll vals
      encodedV1 `shouldBe` Array (V.fromList [object ["type" .= ("bar" :: String)]])
      encodedV2 `shouldBe` Array (V.fromList [object ["type" .= ("bar" :: String)]
                                             ,object ["type" .= ("barPlus" :: String)]])
      encodedAll `shouldBe` object ["1.0" .= [object ["type" .= ("bar" :: String)]]
                                   ,"2.0" .= [object ["type" .= ("bar" :: String)]
                                             ,object ["type" .= ("barPlus" :: String)]]]
