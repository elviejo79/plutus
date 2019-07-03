{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module SchemaSpec where

import           Crypto.Hash  (Digest, SHA256)
import           Data.Proxy   (Proxy (Proxy))
import           Data.Text    (Text)
import           GHC.Generics (Generic)
import           Schema       (Constructor (Constructor, Record), ConstructorName (ConstructorName),
                               DataType (DataType), Reference (Reference), ToSchema, ToTypeName, TypeName (TypeName),
                               toSchema)
import           Test.Hspec   (Spec, describe, it, shouldBe)

spec :: Spec
spec = toSchemaSpec

toSchemaSpec :: Spec
toSchemaSpec =
    describe "To schema" $ do
        it "Int" $
            toSchema (Proxy :: Proxy Int) `shouldBe`
            DataType (TypeName "" "Int") [] []
        it "Integer" $
            toSchema (Proxy :: Proxy Int) `shouldBe`
            DataType (TypeName "" "Int") [] []
        it "String" $
            toSchema (Proxy :: Proxy String) `shouldBe`
            DataType (TypeName "" "String") [] []
        it "Text" $
            toSchema (Proxy :: Proxy Text) `shouldBe`
            DataType (TypeName "" "String") [] []
        it "Hash" $
            toSchema (Proxy @(Digest SHA256)) `shouldBe`
            DataType (TypeName "Crypto.Hash" "Digest") [] []
        it "Array Int" $
            toSchema (Proxy :: Proxy [Int]) `shouldBe`
            DataType
                (TypeName "Data.List" "List")
                []
                [Constructor "List" [Reference (TypeName "" "Int")]]
        it "Maybe String" $
            toSchema (Proxy :: Proxy (Maybe String)) `shouldBe`
            DataType
                (TypeName "GHC.Maybe" "Maybe")
                []
                [ Constructor "Nothing" []
                , Constructor "Just" [Reference (TypeName "" "String")]
                ]
        it "User" $
            toSchema (Proxy :: Proxy User) `shouldBe`
            DataType
                (TypeName "SchemaSpec" "User")
                []
                [ Record
                      (ConstructorName "User")
                      [ ("userName", Reference (TypeName "" "String"))
                      , ("userAge", Reference (TypeName "" "Int"))
                      , ("userAlive", Reference (TypeName "" "Bool"))
                      ]
                ]

data User =
    User
        { userName  :: Text
        , userAge   :: Int
        , userAlive :: Bool
        }
    deriving (Show, Eq, Generic, ToSchema, ToTypeName)
