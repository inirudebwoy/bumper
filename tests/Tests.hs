module Main where

import Bumper
import Test.Hspec


main :: IO ()
main = hspec $ do

  -- versionBumper
  describe "Verify versionBumper happy path" $ do
    it "bump major" $ do
      versionBumper "major" ["1", "0", "0"] `shouldBe` "2.0.0"
    it "bump minor" $ do
      versionBumper "minor" ["1", "0", "0"] `shouldBe` "1.1.0"
    it "bump patch" $ do
      versionBumper "patch" ["1", "0", "0"] `shouldBe` "1.0.1"

  describe "Verify versionBumper empty args" $ do
    it "bump with no part" $ do
      versionBumper "" ["1", "0", "0"] `shouldBe` "1.0.0"
    it "bump with no version" $ do
      versionBumper "minor" [] `shouldBe` ""

  describe "Verify versionBumper with less than three part version" $ do
    it "bump major with two part version" $ do
      versionBumper "major" ["1", "0"] `shouldBe` "2.0"
    it "bump minorwith two part version" $ do
      versionBumper "minor" ["1", "0"] `shouldBe` "1.1"
    it "bump major with one part version" $ do
      versionBumper "major" ["1"] `shouldBe` "2"
    it "bump minor with one part version" $ do
      versionBumper "minor" ["1"] `shouldBe` "1"

  describe "Verify versionBumper with invalid part name" $ do
    it "bump with invalid part name" $ do
      versionBumper "foo" ["1", "0", "0"] `shouldBe` "1.0.0"

  -- addSuffix
  describe "Verify addSuffix replaces suffix in version" $ do
    it "swap rc1 to rc2" $ do
      addSuffix "1.0.0-rc1" "rc2" `shouldBe` "1.0.0-rc2"

  describe "Verify addSuffix adds suffix to version" $ do
    it "add rc1 to version" $ do
      addSuffix "1.0.0" "rc1" `shouldBe` "1.0.0-rc1"

  describe "Verify addSuffix replaces suffix in version with build" $ do
    it "swap rc1 with rc2 but keep build" $ do
      addSuffix "1.0.0-rc1+42" "rc2" `shouldBe` "1.0.0-rc2+42"

  describe "Verify addSuffix adds suffix" $ do
    it "add rc1 to version with build" $ do
      addSuffix "1.0.0+847362" "beta" `shouldBe` "1.0.0-beta+847362"

  -- takeBuild
  describe "Verify takeBuild takes build" $ do
    it "from nothing to nothing" $ do
       takeBuild "" `shouldBe` ""

  describe "Verify takeBuild takes build" $ do
    it "from version with no build" $ do
       takeBuild "1.0.0" `shouldBe` ""

  describe "Verify takeBuild takes build" $ do
    it "from version with no build but with suffix" $ do
       takeBuild "1.0.0-rc1" `shouldBe` ""

  describe "Verify takeBuild takes build" $ do
    it "from version with build but no suffix" $ do
       takeBuild "1.0.0+b01" `shouldBe` "b01"

  describe "Verify takeBuild takes build" $ do
    it "from version with build and suffix" $ do
       takeBuild "1.0.0-rc1+b02" `shouldBe` "b02"

  -- addBuild
  describe "Verify addBuild adds build number" $ do
    it "add build to version" $ do
      addBuild "1.0.0" "11"`shouldBe` "1.0.0+11"

  describe "Verify addBuild adds build number" $ do
    it "add build to version with suffix" $ do
      addBuild "1.0.0-alpha" "11" `shouldBe` "1.0.0-alpha+11"

  describe "Verify addBuild replaces build number" $ do
    it "add build to version" $ do
      addBuild "1.0.0+10" "11"`shouldBe` "1.0.0+11"

  describe "Verify addBuild replaces build number" $ do
    it "add build to version with suffix" $ do
      addBuild "1.0.0-alpha+10" "11" `shouldBe` "1.0.0-alpha+11"

  -- splitVersion
  describe "Verify splitVersion happy path" $ do
    it "split version with three parts" $ do
      splitVersion "1.0.0" `shouldBe` ["1", "0", "0"]
    it "split version with two parts" $ do
      splitVersion "1.0" `shouldBe` ["1", "0"]
    it "split version with one part" $ do
      splitVersion "1" `shouldBe` ["1"]
    it "split version with empty string" $ do
      splitVersion "" `shouldBe` []

  -- replace
  describe "Verify replace" $ do
    it "replaces old version with new" $ do
      replace "1.0.0" "1.0.1" "version=1.0.0" `shouldBe` "version=1.0.1"

    it "replaces both old versions with new" $ do
      replace "1.0.0" "1.1.0" "version=1.0.0#second_version=1.0.0" `shouldBe`
                  "version=1.1.0#second_version=1.1.0"

    it "does not replace if version is not in string" $ do
      replace "1.0.0" "1.0.1" "no version here" `shouldBe` "no version here"

    it "does not change empty string" $ do
      replace "1.0.0" "1.0.1" "" `shouldBe` ""
