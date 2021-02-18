{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.PlantUML.ParserHelperSpec where

import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char as C
import Text.Megaparsec.Char.Lexer 
import Language.PlantUML.Types
import Test.Hspec

import qualified Language.PlantUML.ParserHelper as P (assocParser, lexeme, name, nonQuotedName, pairParser, quotedName, reserved, reservedSymbol, restOfLine, spaceConsumer) 


s1 :: MonadParsec Char T.Text m => m ()
s1 = space1

spec :: Spec
spec = do
    describe "nonQuotedName" $ do
      it "alphabet" $ parseMaybe (P.nonQuotedName) "ab" `shouldBe` (Just "ab")
      it "日本語" $ parseMaybe (P.nonQuotedName) "日本語" `shouldBe` (Just "日本語")
      it "block comment" $ parseMaybe (P.nonQuotedName *> P.spaceConsumer *> P.nonQuotedName) "ab/' '/c" `shouldBe` (Just "c")
      it "line comment" $ parseMaybe (P.nonQuotedName <* P.spaceConsumer) "ab'aaaaaa" `shouldBe` (Just "ab")          
      it "failed case" $ parseMaybe (P.nonQuotedName) "\"abc\"" `shouldBe` Nothing

    describe "quotedName" $ do
      it "alphabet" $ parseMaybe P.quotedName "\"ab\"" `shouldBe` (Just "ab")
      it "日本語" $ parseMaybe P.quotedName "\"日本語\"" `shouldBe` (Just "日本語")
      it "block comment" $ parseMaybe P.quotedName "\"ab /' '/c\"" `shouldBe` (Just "ab /' '/c")
      it "line comment" $ parseMaybe P.quotedName "\"ab 'aaaaaa\"" `shouldBe` (Just "ab 'aaaaaa")

    describe "name" $ do
      it "quoted"     $ parseMaybe P.name "\"ab\"" `shouldBe` (Just "ab")
      it "non quoted" $ parseMaybe P.name "ab" `shouldBe` (Just "ab")

    describe "reserved" $ do
      it "accepts" $ parseMaybe (P.reserved "as") "as" `shouldBe` (Just "as")
      it "accepts two P.reserved"  $ parse (P.reserved "as" *> P.spaceConsumer *> P.reserved "is") "" "as is" `shouldBe`(Right "is")
      it "reject" $ parseMaybe (P.reserved "as") "asis" `shouldBe` Nothing
    describe "reservedSymbol" $ do
      it "accepts" $ parse (P.reservedSymbol "...") "" "..." `shouldBe` (Right "...")
      it "reject" $ parseMaybe (P.reservedSymbol "...") "...." `shouldBe` Nothing

      
    describe "P.pairParser" $ do
      it "OK" $ parse (P.pairParser ("true", pure True)) "" "true" `shouldBe` (Right True)
      it "fail" $ parseMaybe (P.pairParser ("true", pure True)) "True" `shouldBe` Nothing
      
    describe "P.assocParser" $ do
      it "1st match" $ parse (P.assocParser [("true", pure True), ("True", pure True)]) "" "true" `shouldBe` (Right True)
      it "2nd match" $ parse (P.assocParser [("true", pure True), ("True", pure True)]) "" "True" `shouldBe` (Right True)
      it "back track" $ parse (P.assocParser [("true", pure True), ("t", pure True)]) "" "t" `shouldBe` (Right True)
      it "unknown" $ parseMaybe (P.assocParser [("true", pure True), ("True", pure True)]) "False" `shouldBe` Nothing      
      it "should not match" $ parseMaybe (P.assocParser [("t", pure True)]) "true" `shouldBe` Nothing

    describe "restOfLine" $ do
      it "no continuation line" $ do
        parse P.restOfLine "" "group\n" `shouldBe` (Right ["group"])
      it "1 continuation line" $ do
        parse P.restOfLine "" "group\\\nasdf\n" `shouldBe` (Right ["group\\", "asdf"])
