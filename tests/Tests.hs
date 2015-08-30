module Main where

import Bumper
import Test.Hspec


main :: IO ()
main = hspec $ do
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
      versionBumper "minor" [] `shouldBe` "1.0.0"

  describe "Verify versionBumper with less than three part version" $ do
    it "bump with two part version" $ do
      versionBumper "major" ["1", "0"] `shouldBe` "2.0"
    it "bump with one part version" $ do
      versionBumper "major" ["1"] `shouldBe` "2"

  describe "Verify versionBumper with invalid part name" $ do
    it "bump with invalid part name" $ do
      versionBumper "foo" ["1", "0", "0"] `shouldBe` "1.0.0"
