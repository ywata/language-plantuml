{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.PlantUML.ParserSpec where

import qualified Data.Text as T
import Text.Megaparsec
    ( parse, parseMaybe, choice, MonadParsec(try), manyTill, notFollowedBy, lookAhead)
import Text.Megaparsec.Char as C ( space1, string, printChar )
import Text.Megaparsec.Char.Lexer () 
import Language.PlantUML.Types

import Language.PlantUML.Parser
    ( plantUML,
      declSubject,
      parrows,
      declArrow,
      description,
      color,
      reservedAs,
      declNotes,
      declGrouping,
      declCommand,
      skinParamAssoc,
      skinParamParser,
      stereotype)

import Test.Hspec ( describe, it, shouldBe, Spec )

import qualified Language.PlantUML.ParserHelper as P (assocParser, lexeme, name, nonQuotedName, pairParser, quotedName, reserved, restOfLine, spaceConsumer) 


--s1 :: MonadParsec Char T.Text m => m ()
--s1 = space1

spec :: Spec
spec = do
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
      it "Foo1" $ parse (reservedAs) "" "as Foo1"
        `shouldBe` (Right (Just (Alias "Foo1")))

    describe "color" $ do
      it "red" $ parse color "" "#red"
        `shouldBe` (Right (Color Red))
      it "Hex" $ parse color "" "#992233"
        `shouldBe` (Right (HexColor "992233"))

    describe "declSubject" $ do
      it "participant w/o alias" $ parse declSubject "" "participant abc"
        `shouldBe` (Right (Participant (Name "abc") Nothing Nothing Nothing))
      it "participant w alias" $ parse declSubject "" ("participant abc as a")
        `shouldBe` (Right (Participant (Name "abc") (Just (Alias "a")) Nothing Nothing))
      -- more variation
      it "actor w/o alias" $ parse declSubject "" "actor abc"
        `shouldBe` (Right (Actor (Name "abc") Nothing Nothing Nothing))
      it "boundary w/o alias" $ parse declSubject "" "boundary abc"
        `shouldBe` (Right (Boundary (Name "abc") Nothing Nothing Nothing))
      it "control w/o alias" $ parse declSubject "" "control abc"
        `shouldBe` (Right (Control (Name "abc") Nothing Nothing Nothing))
      it "entity w/o alias" $ parse declSubject "" "entity abc"
        `shouldBe` (Right (Entity (Name "abc") Nothing Nothing Nothing))
      it "database w/o alias" $ parse declSubject "" "database abc"
        `shouldBe` (Right (Database (Name "abc") Nothing Nothing Nothing))
      it "database w/o alias" $ parse declSubject "" "database abc"
        `shouldBe` (Right (Database (Name "abc") Nothing Nothing Nothing))
      it "collections w/o alias" $ parse declSubject "" "collections abc"
        `shouldBe` (Right (Collections (Name "abc") Nothing Nothing Nothing))
      it "queue w/o alias" $ parse declSubject "" "queue abc"
        `shouldBe` (Right (Queue (Name "abc") Nothing Nothing Nothing))
      it "consective actors" $ parse declSubject "" "participant participant as Foo \nactor actor as Foo1"
        `shouldBe` (Right (Participant (Name "participant") (Just "Foo") Nothing Nothing))
        
      it "actor with order" $ parse declSubject "" "actor A order 10"
        `shouldBe` (Right (Actor (Name "A") Nothing (Just 10) Nothing))
      it "actor with color" $ parse declSubject "" "actor A #red"
        `shouldBe` (Right (Actor (Name "A") Nothing Nothing (Just (Color Red))))
      it "actor with color and order" $ parse declSubject "" "actor A order 10 #red"
        `shouldBe` (Right (Actor (Name "A") Nothing (Just 10) (Just (Color Red))))
      it "actor with alias, color and color" $ parse declSubject "" "actor A as Foo2 order 10 #red"
        `shouldBe` (Right (Actor (Name "A") (Just "Foo2") (Just 10) (Just (Color Red))))

    describe "stereotype" $ do
      it "one line" $ parse (stereotype (manyTill printChar (string ">>"))) "" "<<one>>>>"
        `shouldBe` (Right (Stereotype ["one>>>>"]))
--      it "multiple lines" $ parse (stereotype (P.nonQuotedName)) ""
--        "<<line\\\n> break stereotype>" `shouldBe` (Right (Stereotype "line break stereotype"))      


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
      it "Bob()" $ parse declArrow "" "Alice -> \"Bob()\" : Hello\n"
        `shouldBe` (Right (Arrow (Just "Alice") "->" (Just "Bob()") (Just [" Hello"])))
      it "Long" $ parse declArrow "" "\"Bob()\" -> Long as \"This is very\nlong\"\n"
        `shouldBe` (Right (Arrow (Just "Bob()") "-->" (Just "Long") (Just ["This is very\nlong"])))
      it "Bob()2" $ parse declArrow "" "Long --> \"Bob()\" : ok\n"
        `shouldBe` (Right (Arrow (Just "Long") "-->" (Just "Bob()") (Just [" ok"])))
    describe "return" $ do
      it "return" $ parse declArrow "" "return\n" `shouldBe` (Right (Return [""]))
    describe "return" $ do
      it "return" $ parse declArrow "" "return   statement\n" `shouldBe` (Right (Return ["   statement"]))

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
      it "autoactivate On" $ parse declCommand "" "autoactivate on" `shouldBe` (Right (AutoActivate On))
      it "autoactivate Off" $ parse declCommand "" "autoactivate off" `shouldBe` (Right (AutoActivate Off))
      it "activate" $ parse declCommand "" "activate A" `shouldBe` (Right (Activate (Name "A") Nothing))
      it "deactivate" $ parse declCommand "" "deactivate B" `shouldBe` (Right (Deactivate (Name "B")))
      it "title" $ parse declCommand "" "title A\n" `shouldBe` (Right (Title ["A"]))
      it "title" $ parse declCommand "" "title A\\a\n" `shouldBe` (Right (Title ["A\\a"]))
      it "title" $ parseMaybe declCommand "title A" `shouldBe` Nothing

    describe "skin parameters" $ do
      it "responseMessageBelowArrow" $
        parse (P.assocParser skinParamAssoc) "" "responseMessageBelowArrow true"
        `shouldBe` (Right (ResponseMessageBelowArrow True))
        
      it "skinparam responseMessageBelowArrow true" $
        parse skinParamParser "" "skinparam responseMessageBelowArrow true"
        `shouldBe` (Right (SkinParameter [ResponseMessageBelowArrow True]))
      it "skinparam responseMessageBelowArrow false" $
        parse skinParamParser "" "skinparam responseMessageBelowArrow false"
        `shouldBe` (Right (SkinParameter [ResponseMessageBelowArrow False]))
      it "skinparam maxMessageSize" $
        parse skinParamParser "" "skinparam maxMessageSize 10"
        `shouldBe` (Right (SkinParameter [MaxMessageSize 10]))

--    describe "declCommand" $ do
--       it "" $ parse plantUML "" "activate A" `shouldBe` (Right (PlantUML [CommandDef (Autonumber Nothing Nothing Nothing)]))

    describe "uml" $ do
      it "@startuml and @enduml" $ parse plantUML "" "@startuml@enduml" `shouldBe` (Right (PlantUML []))
      it "@startuml and @enduml" $ parse plantUML "" "@startuml actor A @enduml"
        `shouldBe` (Right (PlantUML [SubjectDef (Actor (Name "A") Nothing Nothing Nothing)]))
      it "@startuml and @enduml" $ parse plantUML "" "@startuml actor A as a A -> B : aaa\n@enduml"
        `shouldBe` (Right (PlantUML [SubjectDef (Actor (Name "A") (Just "a") Nothing Nothing),
                                     ArrowDef (Arrow (Just "A") "->" (Just "B") (Just [" aaa"]))]))




