{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.PlantUML.ParserHelperSpec where

import qualified Data.Text as T
import Text.Megaparsec hiding(parse, parseMaybe)
import Text.Megaparsec.Char as C
import Text.Megaparsec.Char.Lexer 
import Language.PlantUML.Types
import Test.Hspec

import qualified Language.PlantUML.ParserHelper as P
  (parse,
   parseMaybe,
   assocParser,
   dropContinuationLine,
   ident,
   lexeme,
   nonQuotedName,
   pairParser,
   quotedName,
   reserved,
   reserved',
   reservedSymbol,
   restOfLine,
   spaceConsumer) 


s1 :: MonadParsec Char T.Text m => m ()
s1 = space1

spec :: Spec
spec = do
    describe "printChar" $ do
      it "space" $ P.parse (many printChar) "" "   " `shouldBe` (Right "   ")
    describe "printChar" $ do
      it "many printChar drops newline" $ P.parse (many printChar) "" " {}()<>! \n" `shouldBe` (Right " {}()<>! ")
    describe "nonQuotedName" $ do
      it "alphabet" $ P.parseMaybe (P.nonQuotedName) "ab" `shouldBe` (Just "ab")
      it "日本語" $ P.parseMaybe (P.nonQuotedName) "日本語" `shouldBe` (Just "日本語")
      it "block comment" $ P.parseMaybe (P.nonQuotedName *> P.spaceConsumer *> P.nonQuotedName) "ab/' '/c" `shouldBe` (Just "c")
      it "line comment" $ P.parseMaybe (P.nonQuotedName <* P.spaceConsumer) "ab'aaaaaa" `shouldBe` (Just "ab")          
      it "failed case" $ P.parseMaybe (P.nonQuotedName) "\"abc\"" `shouldBe` Nothing

    describe "quotedName" $ do
      it "alphabet" $ P.parseMaybe P.quotedName "\"ab\"" `shouldBe` (Just "ab")
      it "日本語" $ P.parseMaybe P.quotedName "\"日本語\"" `shouldBe` (Just "日本語")
      it "block comment" $ P.parseMaybe P.quotedName "\"ab /' '/c\"" `shouldBe` (Just "ab /' '/c")
      it "line comment" $ P.parseMaybe P.quotedName "\"ab 'aaaaaa\"" `shouldBe` (Just "ab 'aaaaaa")

    describe "reserved" $ do
      it "accepts newline" $ P.parseMaybe (P.reserved "as") "as  \n" `shouldBe` (Just "as")
      it "accepts" $ P.parseMaybe (P.reserved "as") "as" `shouldBe` (Just "as")
      it "accepts two P.reserved"  $ P.parse (P.reserved "as" *> P.spaceConsumer *> P.reserved "is") "" "as is" `shouldBe`(Right "is")
      it "reject" $ P.parseMaybe (P.reserved "as") "asis" `shouldBe` Nothing
    describe "reservedSymbol" $ do
      it "accepts" $ P.parse (P.reservedSymbol "...") "" "..." `shouldBe` (Right "...")
      it "reject" $ P.parseMaybe (P.reservedSymbol "...") "...." `shouldBe` Nothing

      
    describe "P.pairParser" $ do
      it "OK" $ P.parse (P.pairParser ("true", pure True)) "" "true" `shouldBe` (Right True)
      it "fail" $ P.parseMaybe (P.pairParser ("true", pure True)) "True" `shouldBe` Nothing
      
    describe "P.assocParser" $ do
      it "1st match" $ P.parse (P.assocParser [("true", pure True), ("True", pure True)]) "" "true" `shouldBe` (Right True)
      it "2nd match" $ P.parse (P.assocParser [("true", pure True), ("True", pure True)]) "" "True" `shouldBe` (Right True)
      it "back track" $ P.parse (P.assocParser [("true", pure True), ("t", pure True)]) "" "t" `shouldBe` (Right True)
      it "unknown" $ P.parseMaybe (P.assocParser [("true", pure True), ("True", pure True)]) "False" `shouldBe` Nothing      
      it "should not match" $ P.parseMaybe (P.assocParser [("t", pure True)]) "true" `shouldBe` Nothing

    describe "restOfLine" $ do
      it "no continuation line" $ do
        P.parse P.restOfLine "" "group\n" `shouldBe` (Right "group")
      it "1 continuation line" $ do
        P.parse P.restOfLine "" "group\\\nasdf\n" `shouldBe` (Right "groupasdf")
      it "space" $ do
        P.parse P.restOfLine "" " \n" `shouldBe` (Right " ")
      it "empty line" $ do
        P.parse P.restOfLine "" "\n" `shouldBe` (Right "")
      it "space and string" $ do
        P.parse P.restOfLine "" " string\n" `shouldBe` (Right " string")
    describe "reserved' and restOfLine" $ do        
      it "reserved' empty + restOfLine" $ do
        P.parse (P.reserved' "title" >> P.restOfLine) "" "title\n" `shouldBe` (Right "")
      it "reserved' space+ restOfLine" $ do
        P.parse (P.reserved' "title" >> P.restOfLine) "" "title \n" `shouldBe` (Right " ")
      it "reserved' string + restOfLine" $ do
        P.parse (P.reserved' "title" >> P.restOfLine) "" "title string\n" `shouldBe` (Right " string")


    describe "ident" $ do
      it "idnet" $ do
        P.parse P.ident "" "asdf" `shouldBe` (Right "asdf")
      it "idnet with @" $ do
        P.parse P.ident "" "@startuml" `shouldBe` (Right "@startuml")
    describe "dropContinuationLine" $ do
      it "\\n" $ do
        P.dropContinuationLine "\n" `shouldBe` "\n"
      it "\\r\\n" $ do
        P.dropContinuationLine "\r\n " `shouldBe` "\r\n "
        
      it "empty line" $ do
        P.parse (many printChar) "" ""  `shouldBe` (Right ("" :: String))
      it "no new line" $ do
        P.parse (many printChar) "" ("no new line"::T.Text) `shouldBe` (Right "no new line")
      it "new line" $ do
        P.parse (many (printChar <|> controlChar)) "" ("new line\n" ::T.Text) `shouldBe` (Right "new line\n")
      it "cr lf" $ do
        P.parse (many (printChar<|> controlChar))  "" "cr lf\r\n" `shouldBe` (Right "cr lf\r\n")
      it "cont lf" $ do
        P.parse (many (printChar<|> controlChar)) "" "\\\n" `shouldBe` (Right "")
      it "cont cr lf" $ do
        P.parse (many (printChar<|> controlChar)) "" "\\\r\n" `shouldBe` (Right "")
      it "concatenate lf" $ do
        P.parse (many (printChar<|> controlChar)) "" "a\\\nb" `shouldBe` (Right "ab")
      it "concatenate cr lf" $ do
        P.parse (many (printChar<|> controlChar)) "" "a\\\r\nb" `shouldBe` (Right "ab")
      it "double \\" $ do
        P.parse (many (printChar<|> controlChar)) "" "a\\\\\r\nb" `shouldBe` (Right "a\\\r\nb")


