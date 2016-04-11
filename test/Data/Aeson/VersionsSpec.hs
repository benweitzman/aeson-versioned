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

spec :: Spec
spec  = do
  describe "versions" $ do
    it "serializes two versions" $ do
      let val = Identity (Foo 5 "five")
          Just encodedV1 = flip serialize (Identity (Foo 5 "five")) =<< (M.lookup v1 serializers)
          Just encodedV2 = flip serialize (Identity (Foo 5 "five")) =<< (M.lookup v2 serializers)
      encodedV1 `shouldBe` object ["a" .= (5::Int)]
      encodedV2 `shouldBe` object ["a" .= (5::Int), "b" .= ("five" :: String)]
    it "serializes all versions" $ do
      let encoded = serializeAll (Identity (Foo 5 "five"))
      encoded `shouldBe` object [ "1.0" .= object ["a" .= (5 :: Int)]
                                , "2.0" .= object ["a" .= (5 :: Int)
                                                  ,"b" .= ("five" :: String)
                                                  ]
                                ]
