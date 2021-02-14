{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.PlantUML.ParserHelperSpec where

import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char as C
import Text.Megaparsec.Char.Lexer 
import Language.PlantUML.Types
import Test.Hspec

import qualified Language.PlantUML.ParserHelper as P (assocParser, lexeme, name, nonQuotedName, pairParser, quotedName, reserved, restOfLine, spaceConsumer) 


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
      
    describe "reservedAs" $ do
      it "use alias" $ parse reservedAs "" "as abc" `shouldBe` (Right (Just (Alias "abc")))
      it "no alias" $ parseMaybe reservedAs "as" `shouldBe` Nothing
      it "works after alias" $ parse (P.lexeme reservedAs) "" "as abc " `shouldBe` (Right (Just (Alias "abc")))
      it "works after alias" $ parse (P.lexeme reservedAs *> string "string") "" "as abc string" `shouldBe` (Right "string")
      it "use alias" $ parseMaybe reservedAs "" `shouldBe` (Just Nothing)

    describe "space + reservedAs" $ do
      it "use alias" $ parse (try $ P.spaceConsumer *> reservedAs) "" " as abc" `shouldBe` (Right (Just (Alias "abc")))
      it "no alias" $ parseMaybe (try $ P.spaceConsumer *> reservedAs) " as" `shouldBe` Nothing
      it "works after alias" $ parse (try $ P.spaceConsumer *> P.lexeme reservedAs) "" "  as abc " `shouldBe` (Right (Just (Alias "abc")))
      it "works after alias" $ parse (try (P.spaceConsumer *> P.lexeme reservedAs) *> string "string") "" "  as abc string"
        `shouldBe` (Right "string")
      it "empty" $ parseMaybe (try (P.spaceConsumer *> reservedAs)) " "
        `shouldBe` (Just Nothing)

    describe "P.pairParser" $ do
      it "OK" $ parse (P.pairParser ("true", pure True)) "" "true" `shouldBe` (Right True)
      it "fail" $ parseMaybe (P.pairParser ("true", pure True)) "True" `shouldBe` Nothing
      
    describe "P.assocParser" $ do
      it "1st match" $ parse (P.assocParser [("true", pure True), ("True", pure True)]) "" "true" `shouldBe` (Right True)
      it "2nd match" $ parse (P.assocParser [("true", pure True), ("True", pure True)]) "" "True" `shouldBe` (Right True)
      it "back track" $ parse (P.assocParser [("true", pure True), ("t", pure True)]) "" "t" `shouldBe` (Right True)
      it "unknown" $ parseMaybe (P.assocParser [("true", pure True), ("True", pure True)]) "False" `shouldBe` Nothing      
      it "should not match" $ parseMaybe (P.assocParser [("t", pure True)]) "true" `shouldBe` Nothing

    describe "color" $ do
      it "red" $ parse color "" "#red"
        `shouldBe` (Right (Color Red))
      it "Hex" $ parse color "" "#992233"
        `shouldBe` (Right (HexColor "992233"))

    describe "declSubject" $ do
      it "participant w/o alias" $ parse declSubject "" "participant abc"
        `shouldBe` (Right (Participant (Name "abc") Nothing))
      it "participant w alias" $ parse declSubject "" ("participant abc as a")
        `shouldBe` (Right (Participant (Name "abc") (Just (Alias "a"))))
      -- more variation
      it "actor w/o alias" $ parse declSubject "" "actor abc"
        `shouldBe` (Right (Actor (Name "abc") Nothing))
      it "boundary w/o alias" $ parse declSubject "" "boundary abc"
        `shouldBe` (Right (Boundary (Name "abc") Nothing))
      it "control w/o alias" $ parse declSubject "" "control abc"
        `shouldBe` (Right (Control (Name "abc") Nothing))
      it "entity w/o alias" $ parse declSubject "" "entity abc"
        `shouldBe` (Right (Entity (Name "abc") Nothing))
      it "database w/o alias" $ parse declSubject "" "database abc"
        `shouldBe` (Right (Database (Name "abc") Nothing))
      it "database w/o alias" $ parse declSubject "" "database abc"
        `shouldBe` (Right (Database (Name "abc") Nothing))
      it "collections w/o alias" $ parse declSubject "" "collections abc"
        `shouldBe` (Right (Collections (Name "abc") Nothing))
      it "queue w/o alias" $ parse declSubject "" "queue abc"
        `shouldBe` (Right (Queue (Name "abc") Nothing))

    describe "arrow1" $ do
      it "arrow" $ parse (choice parrows) "" "->" `shouldBe` (Right "->")

    describe "arrow" $ do
      it "A -> B : a b c" $ parse declArrow "" "A -> B :a b c\n "
        `shouldBe` (Right (Arrow (Just "A") "->" (Just "B") (Just ["a b c"])))
      it "A->B" $ parse declArrow "" "A->B "
        `shouldBe` (Right (Arrow (Just "A") "->" (Just "B") Nothing))
      it "-> B" $ parse declArrow "" "-> B "
        `shouldBe` (Right (Arrow Nothing "->" (Just "B") Nothing))
      it "-> B" $ parse declArrow "" "-> B : a b c\n"
        `shouldBe` (Right (Arrow Nothing "->" (Just "B") (Just [" a b c"])))
      it "A->" $ parse declArrow "" "A -> : a b c\n"
        `shouldBe` (Right (Arrow (Just "A") "->" Nothing (Just [" a b c"])))

    describe "description" $ do
      it "one line" $ parse description "" "abc\n" `shouldBe` (Right ["abc"])
      it "two line" $ parse description "" "abc\\\ndef\n" `shouldBe` (Right ["abc\\", "def"])      


    describe "notes" $ do
      it "note left oneline" $ parse declNotes "" ("note left : right\n")
        `shouldBe` (Right (NoteLeft Nothing ["right"]))
      it "note right oneline" $ parse declNotes "" ("note right :left\n")
        `shouldBe` (Right (NoteRight Nothing ["left"]))
      it "note over oneline 1" $ parse declNotes "" ("note over A : A\n")
        `shouldBe` (Right (NoteOver "A" Nothing [" A"]))      
      it "note over oneline 2" $ parse declNotes "" ("note over A, B :A B\n")
        `shouldBe` (Right (NoteOver "A" (Just "B") ["A B"]))
      it "note over twolines 1" $ parse declNotes "" ("note over A of\nA B\nend note\n")
        `shouldBe` (Right (NoteOver "A" Nothing ["A B"]))
      it "note over twolines 2" $ parse declNotes "" ("note over A, B of\nA B\nend note\n")
        `shouldBe` (Right (NoteOver "A" (Just "B") ["A B"]))
    describe "restOfLine" $ do
      it "no continuation line" $ do
        parse P.restOfLine "" "group\n" `shouldBe` (Right ["group"])
      it "1 continuation line" $ do
        parse P.restOfLine "" "group\\\nasdf\n" `shouldBe` (Right ["group\\", "asdf"])
{-        
    describe "doubleLabels" $ do
      it "one label" $ do
        parseMaybe doubleLabels "aaaa bbbb cccc dddd" `shouldBe` Nothing
      it "two labels" $ do
        parseMaybe doubleLabels "aaaa bbbb cccc [dddd]" `shouldBe` (Just [])
      it "ignore psuedo second label" $ do
        parseMaybe doubleLabels "aaaa bbbb cccc [dddd] a" `shouldBe` Nothing
      it "ignore double ]]" $ do
        parseMaybe doubleLabels "aaaa bbbb cccc [dddd]] " `shouldBe` (Just [])
-}    
    describe "grouping" $ do
      it "label 1" $ parse declGrouping "" "group A\nend group\n"
        `shouldBe` (Right (Grouping Group ["A"] [[]]))
      it "label 2" $ parse declGrouping "" "group label1[label 2]\nend group\n"
        `shouldBe` (Right (Grouping Group ["label1[label 2]"] [[]]))
      it "label multiple lines" $ parse declGrouping "" "group a\\b\\c\nend group"
        `shouldBe` (Right (Grouping Group ["a\\b\\c"] [[]]))

      
      it "just group" $ parse declGrouping "" "group A\nA->B\nend group\n"
        `shouldBe` (Right (Grouping Group ["A"] [[ArrowDef (Arrow (Just "A") "->" (Just "B") Nothing)]]))
      it "alt else" $ parse declGrouping "" "alt a\nA->B: A -> B\nelse\nB->C: B-> C\nend alt\n"
        `shouldBe` ( Right (Grouping Alt ["a"]
                            [[ArrowDef (Arrow (Just "A") "->" (Just "B") (Just [" A -> B"]))],
                             [ArrowDef (Arrow (Just "B") "->" (Just "C") (Just [" B-> C"]))]]))
      it "opt else else" $ parse declGrouping "" "opt a\nA->B: A -> B\nelse\nB->C: B-> C\nelse C->D: C -> D\nend opt\n"
        `shouldBe` ( Right (Grouping Opt ["a"]
                            [[ArrowDef (Arrow (Just "A") "->" (Just "B") (Just [" A -> B"]))],
                             [ArrowDef (Arrow (Just "B") "->" (Just "C") (Just [" B-> C"]))],
                             [ArrowDef (Arrow (Just "C") "->" (Just "D") (Just [" C -> D"]))]]))
      
    describe "command" $ do
      it "autonumber" $ parse declCommand "" "autonumber" `shouldBe` (Right (Autonumber Nothing Nothing Nothing))
      it "autonumber 10" $ parse declCommand "" "autonumber 10"
        `shouldBe` (Right (Autonumber (Just 10) Nothing Nothing))      
      it "autonumber 10 20" $ parse declCommand "" "autonumber 10 20"
        `shouldBe` (Right (Autonumber (Just 10) (Just 20) ( Nothing)))
                          
      it "autonumber 10 20 30" $ parse declCommand "" "autonumber 10 20 30"
        `shouldBe` (Right (Autonumber (Just 10) (Just 20) (Just 30)))     
    describe "uml" $ do
      it "@startuml and @enduml" $ parse plantUML "" "@startuml@enduml" `shouldBe` (Right (PlantUML []))
      it "@startuml and @enduml" $ parse plantUML "" "@startuml actor A @enduml"
        `shouldBe` (Right (PlantUML [SubjectDef (Actor (Name "A") Nothing)]))
      it "@startuml and @enduml" $ parse plantUML "" "@startuml actor A as a A -> B : aaa\n@enduml"
        `shouldBe` (Right (PlantUML [SubjectDef (Actor (Name "A") (Just "a")),
                                     ArrowDef (Arrow (Just "A") "->" (Just "B") (Just [" aaa"]))]))




