module NamerSpec (spec) where

import Test.Hspec
import Control.Exception (evaluate)
import Namer

spec :: Spec
spec = do
  describe "cp2name" $ do
    it "uses corrected names, if present" $ do
      cp2name 65048 `shouldBe` "PRESENTATION FORM FOR VERTICAL RIGHT WHITE LENTICULAR BRACKET"
    it "implements NR1" $ do
      cp2name 54491 `shouldBe` "HANGUL SYLLABLE PWILH"
    it "implements NR2" $ do
      cp2name 94208 `shouldBe` "TANGUT IDEOGRAPH-17000"
    it "names basic multilingual codepoints" $ do
      cp2name 8205 `shouldBe` "ZERO WIDTH JOINER"
    it "names supplemental codepoints" $ do
      cp2name 92417 `shouldBe` "BAMUM LETTER PHASE-D SHEUAE"
    it "names control codepoints" $ do
      cp2name 0 `shouldBe` "NULL"
    it "names figment codepoints" $ do
      cp2name 128 `shouldBe` "PADDING CHARACTER"
    it "labels private-use codepoints" $ do
      cp2name 1048576 `shouldBe` "<private-use>"
    it "labels surrogate codepoints" $ do
      cp2name 56064 `shouldBe` "<surrogate>"
    it "labels noncharacters" $ do
      cp2name 64992 `shouldBe` "<noncharacter>"
    it "defaults to reserved" $ do
      cp2name 458835 `shouldBe` "<reserved>"
    it "errors on points beyond the Unicode codespace" $ do
      evaluate (cp2name 1114113) `shouldThrow` errorCall "Not a valid codepoint"