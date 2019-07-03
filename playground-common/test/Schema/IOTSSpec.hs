{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Schema.IOTSSpec where

import           Data.Algorithm.Diff          (Diff, getGroupedDiff)
import           Data.Algorithm.DiffOutput    (ppDiff)
import           Data.Function                (on)
import           Data.Proxy                   (Proxy (Proxy))
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import           GHC.Generics                 (Generic)
import           Paths_playground_common      (getDataFileName)
import           Schema                       (Constructor (Constructor, Record), ConstructorName (ConstructorName),
                                               DataType (DataType), Reference (Reference), ToSchema, ToTypeName,
                                               toSchema)
import           Schema.IOTS                  (export)
import           Test.Hspec                   (Spec, describe, hspec, it, shouldBe)
import           Test.HUnit                   (Assertion, assertBool)
import           Text.PrettyPrint.Leijen.Text (displayTStrict, renderPretty)

k :: IO ()
k = hspec spec

spec :: Spec
spec = toIOTSSpec

toIOTSSpec :: Spec
toIOTSSpec =
    describe "Export to IOTS format" $
    it "Should export a sample user" $
    exportsAs (Proxy @User) "test/Schema/IOTS/user.iots"

exportsAs :: ToSchema a => a -> FilePath -> IO ()
exportsAs proxy filename = do
    file <- T.readFile =<< getDataFileName filename
    let exported = displayTStrict $ renderPretty 0.8 200 $ export proxy
    exported `shouldBePrettyDiff` file

shouldBePrettyDiff :: Text -> Text -> Assertion
shouldBePrettyDiff a b =
    assertBool (ppDiff (diffLines a b)) (T.stripEnd a == T.stripEnd b)
  where
    diffLines :: Text -> Text -> [Diff [String]]
    diffLines = getGroupedDiff `on` lines . T.unpack

data User =
    User
        { userId :: Int
        , name   :: String
        }
    deriving (Show, Eq, Generic)
    deriving anyclass (ToSchema)
