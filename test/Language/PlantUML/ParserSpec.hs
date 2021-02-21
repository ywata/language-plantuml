{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.PlantUML.ParserSpec where

import qualified Data.Text as T
import Text.Megaparsec 
    ( choice, MonadParsec(try), manyTill, notFollowedBy, lookAhead, ParseErrorBundle(..), Parsec(..))
import qualified Text.Megaparsec  as M (parse, parseMaybe)


import Text.Megaparsec.Char as C ( space1, string, printChar )
import Text.Megaparsec.Char.Lexer () 
import Language.PlantUML.Types

import Language.PlantUML.Parser
    ( plantUML,
      declSubject,
      declArrow,
      color,
      reservedAs,
      declNotes,
      declGrouping,
      declCommand,
      skinParamAssoc,
      skinParamParser,
      stereotype,
      arrow
    )

import Test.Hspec ( describe, it, shouldBe, Spec )

import qualified Language.PlantUML.ParserHelper as P (
  parse,
  parseMaybe,
  assocParser,
  dropContinuationLine,
  lexeme,
  name,
  nonQuotedName,
  pairParser,
  quotedName,
  reserved,
  restOfLine,
  spaceConsumer) 

    

--s1 :: MonadParsec Char T.Text m => m ()
--s1 = space1

spec :: Spec
spec = do

    describe "reservedAs" $ do
      it "use alias" $ P.parse reservedAs "" "as abc" `shouldBe` (Right (Just (Alias "abc")))
      it "no alias" $ P.parseMaybe reservedAs "as" `shouldBe` Nothing
      it "works after alias" $ P.parse (P.lexeme reservedAs) "" "as abc " `shouldBe` (Right (Just (Alias "abc")))

      
      
      it "works after alias" $ P.parse (P.lexeme reservedAs *> string "string") "" "as abc string" `shouldBe` (Right "string")
      it "use alias" $ P.parseMaybe reservedAs "" `shouldBe` (Just Nothing)

    describe "space + reservedAs" $ do
      it "use alias" $ P.parse (try $ P.spaceConsumer *> reservedAs) "" " as abc" `shouldBe` (Right (Just (Alias "abc")))
      it "no alias" $ P.parseMaybe (try $ P.spaceConsumer *> reservedAs) " as" `shouldBe` Nothing
      it "works after alias" $ P.parse (try $ P.spaceConsumer *> P.lexeme reservedAs) "" "  as abc " `shouldBe` (Right (Just (Alias "abc")))
      it "works after alias" $ P.parse (try (P.spaceConsumer *> P.lexeme reservedAs) *> string "string") "" "  as abc string"
        `shouldBe` (Right "string")
      it "empty" $ P.parseMaybe (try (P.spaceConsumer *> reservedAs)) " "
        `shouldBe` (Just Nothing)
      it "Foo1" $ P.parse (reservedAs) "" "as Foo1"
        `shouldBe` (Right (Just (Alias "Foo1")))


    describe "color" $ do
      it "red" $ P.parse color "" "#red"
        `shouldBe` (Right (Color Red))
      it "Hex" $ P.parse color "" "#992233"
        `shouldBe` (Right (HexColor "992233"))

    describe "declSubject" $ do
      it "participant w/o alias" $ P.parse declSubject "" "participant abc"
        `shouldBe` (Right (Participant (Name "abc") Nothing Nothing Nothing))
      it "participant w alias" $ P.parse declSubject "" ("participant abc as a")
        `shouldBe` (Right (Participant (Name "abc") (Just (Alias "a")) Nothing Nothing))
      -- more variation
      it "actor w/o alias" $ P.parse declSubject "" "actor abc"
        `shouldBe` (Right (Actor (Name "abc") Nothing Nothing Nothing))
      it "boundary w/o alias" $ P.parse declSubject "" "boundary abc"
        `shouldBe` (Right (Boundary (Name "abc") Nothing Nothing Nothing))
      it "control w/o alias" $ P.parse declSubject "" "control abc"
        `shouldBe` (Right (Control (Name "abc") Nothing Nothing Nothing))
      it "entity w/o alias" $ P.parse declSubject "" "entity abc"
        `shouldBe` (Right (Entity (Name "abc") Nothing Nothing Nothing))
      it "database w/o alias" $ P.parse declSubject "" "database abc"
        `shouldBe` (Right (Database (Name "abc") Nothing Nothing Nothing))
      it "database w/o alias" $ P.parse declSubject "" "database abc"
        `shouldBe` (Right (Database (Name "abc") Nothing Nothing Nothing))
      it "collections w/o alias" $ P.parse declSubject "" "collections abc"
        `shouldBe` (Right (Collections (Name "abc") Nothing Nothing Nothing))
      it "queue w/o alias" $ P.parse declSubject "" "queue abc"
        `shouldBe` (Right (Queue (Name "abc") Nothing Nothing Nothing))
      it "consective actors" $ P.parse declSubject "" "participant participant as Foo \nactor actor as Foo1"
        `shouldBe` (Right (Participant (Name "participant") (Just "Foo") Nothing Nothing))
        
      it "actor with order" $ P.parse declSubject "" "actor A order 10"
        `shouldBe` (Right (Actor (Name "A") Nothing (Just 10) Nothing))
      it "actor with color" $ P.parse declSubject "" "actor A #red"
        `shouldBe` (Right (Actor (Name "A") Nothing Nothing (Just (Color Red))))
      it "actor with color and order" $ P.parse declSubject "" "actor A order 10 #red"
        `shouldBe` (Right (Actor (Name "A") Nothing (Just 10) (Just (Color Red))))
      it "actor with alias, color and color" $ P.parse declSubject "" "actor A as Foo2 order 10 #red"
        `shouldBe` (Right (Actor (Name "A") (Just "Foo2") (Just 10) (Just (Color Red))))

--    describe "stereotype" $ do
--      it "one line" $ P.parse (stereotype (manyTill printChar (string ">>"))) "" "<<one>>>>"
--        `shouldBe` (Right (Stereotype ["one>>>>"]))
--      it "multiple lines" $ parse (stereotype (P.nonQuotedName)) ""
--        "<<line\\\n> break stereotype>" `shouldBe` (Right (Stereotype "line break stereotype"))      


--    describe "arrow1" $ do
--      it "arrow" $ parse (choice parrows) "" "->" `shouldBe` (Right "->")

    describe "arrow" $ do
      it "A -> B : a b c" $ P.parse declArrow "" "A -> B :a b c\n "
        `shouldBe` (Right (Arrow2 (Just "A") (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just "B") (Just "a b c")))
      it "A->B" $ P.parse declArrow "" "A->B "
        `shouldBe` (Right (Arrow2 (Just "A") (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just "B") Nothing))
      it "-> B" $ P.parse declArrow "" "-> B "
        `shouldBe` (Right (Arrow2 Nothing (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just "B") Nothing))
      it "-> B" $ P.parse declArrow "" "-> B : a b c\n"
        `shouldBe` (Right (Arrow2 Nothing (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just "B") (Just " a b c")))
      it "A->" $ P.parse declArrow "" "A -> : a b c\n"
        `shouldBe` (Right (Arrow2 (Just "A") (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) Nothing (Just " a b c")))
      it "Bob()" $ P.parse declArrow "" "Alice -> \"Bob()\" : Hello\n"
        `shouldBe` (Right (Arrow2 (Just "Alice") (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just "Bob()") (Just " Hello")))
      it "Long" $ P.parse declArrow "" "\"Bob()\" -> Long as \"This is very\nlong\"\n"
        `shouldBe` (Right (Arrow2 (Just "Bob()") (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just "Long") Nothing))
      it "Bob()2" $ P.parse declArrow "" "Long --> \"Bob()\" : ok\n"
        `shouldBe` (Right (Arrow2 (Just "Long") (Arr Nothing (Shaft (Just "--") Nothing Nothing) (Just ">")) (Just "Bob()") (Just " ok")))
      it "A -> \"B\" as b" $ P.parse declArrow "" "-> B "
        `shouldBe`
        (Right (Arrow2 (Just "A")
                        (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just "B")
                        (Just "b")))


    describe "return only" $ do
      it "return" $ P.parse declArrow "" "return\n" `shouldBe` (Right (Return Nothing))
    describe "return with space only" $ do
      it "return" $ P.parse declArrow "" "return  \n" `shouldBe` (Right (Return Nothing))
    describe "return with message" $ do
      it "return" $ P.parse declArrow "" "return   statement\n" `shouldBe` (Right (Return (Just "statement")))

    describe "arrow color" $ do
      it "->" $ P.parse arrow "" "->" `shouldBe` (Right (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")))
      it "<-" $ P.parse arrow "" "<-" `shouldBe` (Right (Arr (Just "<") (Shaft (Just "-") Nothing Nothing) Nothing))
      it "->" $ P.parse arrow "" "<->" `shouldBe` (Right (Arr (Just "<") (Shaft (Just "-") Nothing Nothing) (Just ">")))
      it "-->" $ P.parse arrow "" "-->" `shouldBe` (Right (Arr Nothing (Shaft (Just "--") Nothing Nothing) (Just ">")))
      it "[#red]-->" $ P.parse arrow "" "[#red]-->"
        `shouldBe` (Right (Arr Nothing (Shaft Nothing (Just (Color Red)) (Just "--")) (Just ">")))
      it "-[#red]->" $ P.parse arrow "" "-[#red]->"
        `shouldBe` (Right (Arr Nothing (Shaft (Just "-") (Just (Color Red)) (Just "-")) (Just ">")))
      it "--[#red]>" $ P.parse arrow "" "--[#red]>"
        `shouldBe` (Right (Arr Nothing (Shaft (Just "--") (Just (Color Red)) Nothing) (Just ">")))
      it "o--[#red]>" $ P.parse arrow "" "o--[#red]>x"
        `shouldBe` (Right (Arr (Just "o") (Shaft (Just "--") (Just (Color Red)) Nothing) (Just ">x")))
      it "--->" $ P.parse arrow "" "--->" `shouldBe` (Right (Arr Nothing (Shaft (Just "---") Nothing Nothing) (Just ">")))
      it "<--->" $ P.parse arrow "" "<--->" `shouldBe` (Right (Arr (Just "<") (Shaft (Just "---") Nothing Nothing) (Just ">")))      

    describe "notes" $ do
      it "note left oneline" $ P.parse declNotes "" ("note left : right\n")
        `shouldBe` (Right (NoteLeft Nothing ["right"]))
      it "note right oneline" $ P.parse declNotes "" ("note right :left\n")
        `shouldBe` (Right (NoteRight Nothing ["left"]))
      it "note over oneline 1" $ P.parse declNotes "" ("note over A : A\n")
        `shouldBe` (Right (NoteOver "A" Nothing [" A"]))      
      it "note over oneline 2" $ P.parse declNotes "" ("note over A, B :A B\n")
        `shouldBe` (Right (NoteOver "A" (Just "B") ["A B"]))
      it "note over twolines 1" $ P.parse declNotes "" ("note over A of\nA B\nend note\n")
        `shouldBe` (Right (NoteOver "A" Nothing ["A B"]))
      it "note over twolines 2" $ P.parse declNotes "" ("note over A, B of\nA B\nend note\n")
        `shouldBe` (Right (NoteOver "A" (Just "B") ["A B"]))
{-
    describe "doubleLabels" $ do
      it "one label" $ do
        P.parseMaybe doubleLabels "aaaa bbbb cccc dddd" `shouldBe` Nothing
      it "two labels" $ do
        P.parseMaybe doubleLabels "aaaa bbbb cccc [dddd]" `shouldBe` (Just [])
      it "ignore psuedo second label" $ do
        P.parseMaybe doubleLabels "aaaa bbbb cccc [dddd] a" `shouldBe` Nothing
      it "ignore double ]]" $ do
        P.parseMaybe doubleLabels "aaaa bbbb cccc [dddd]] " `shouldBe` (Just [])
-}

    describe "grouping" $ do
      it "label 1" $ P.parse declGrouping "" "group A\nend group\n"
        `shouldBe` (Right (Grouping Group "A" [[]]))
      it "label 2" $ P.parse declGrouping "" "group label1[label 2]\nend group\n"
        `shouldBe` (Right (Grouping Group "label1[label 2]" [[]]))
      it "label multiple lines" $ P.parse declGrouping "" "group a\\b\\c\nend group"
        `shouldBe` (Right (Grouping Group "a\\b\\c" [[]]))

      it "just group" $ P.parse declGrouping "" "group A\nA->B\nend group\n"
        `shouldBe`  (Right (Grouping Group "A" [[ArrowDef (Arrow2 (Just "A") (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just "B") Nothing)]]))
      it "alt else" $ P.parse declGrouping "" "alt a\nA->B: A -> B\nelse\nB->C: B-> C\nend alt\n"
        `shouldBe` ( Right (Grouping Alt "a" [[ArrowDef (Arrow2 (Just "A") (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just "B") (Just " A -> B"))],[ArrowDef (Arrow2 (Just "B") (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just "C") (Just " B-> C"))]]))
      it "opt else else" $ P.parse declGrouping "" "opt a\nA->B: A -> B\nelse\nB->C: B-> C\nelse C->D: C -> D\nend opt\n"
        `shouldBe` (Right (Grouping Opt "a" [[ArrowDef (Arrow2 (Just "A") (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just "B") (Just " A -> B"))],[ArrowDef (Arrow2 (Just "B") (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just "C") (Just " B-> C"))],[ArrowDef (Arrow2 (Just "C") (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just "D") (Just " C -> D"))]]))


    describe "command" $ do
      it "autonumber" $ P.parse declCommand "" "autonumber" `shouldBe` (Right (Autonumber Nothing Nothing Nothing))
      it "autonumber 10" $ P.parse declCommand "" "autonumber 10"
        `shouldBe` (Right (Autonumber (Just 10) Nothing Nothing))      
      it "autonumber 10 20" $ P.parse declCommand "" "autonumber 10 20"
        `shouldBe` (Right (Autonumber (Just 10) (Just 20) ( Nothing)))
                          
      it "autonumber 10 20 30" $ P.parse declCommand "" "autonumber 10 20 30"
        `shouldBe` (Right (Autonumber (Just 10) (Just 20) (Just 30)))     
      it "autoactivate On" $ P.parse declCommand "" "autoactivate on" `shouldBe` (Right (AutoActivate On))
      it "autoactivate Off" $ P.parse declCommand "" "autoactivate off" `shouldBe` (Right (AutoActivate Off))
      it "activate" $ P.parse declCommand "" "activate A" `shouldBe` (Right (Activate (Name "A") Nothing))
      it "deactivate" $ P.parse declCommand "" "deactivate B" `shouldBe` (Right (Deactivate (Name "B")))
      it "title" $ P.parse declCommand "" "title A\n" `shouldBe` (Right (Title "A"))
      it "title" $ P.parse declCommand "" "title A\\a\n" `shouldBe` (Right (Title "A\\a"))
      it "title" $ P.parseMaybe declCommand "title A" `shouldBe` Nothing

    describe "skin parameters" $ do
      it "responseMessageBelowArrow" $
        P.parse (P.assocParser skinParamAssoc) "" "responseMessageBelowArrow true"
        `shouldBe` (Right (ResponseMessageBelowArrow True))
        
      it "skinparam responseMessageBelowArrow true" $
        P.parse skinParamParser "" "skinparam responseMessageBelowArrow true"
        `shouldBe` (Right (SkinParameter [ResponseMessageBelowArrow True]))
      it "skinparam responseMessageBelowArrow false" $
        P.parse skinParamParser "" "skinparam responseMessageBelowArrow false"
        `shouldBe` (Right (SkinParameter [ResponseMessageBelowArrow False]))
      it "skinparam maxMessageSize" $
        P.parse skinParamParser "" "skinparam maxMessageSize 10"
        `shouldBe` (Right (SkinParameter [MaxMessageSize 10]))


    describe "uml" $ do
      it "@startuml" $ P.parseMaybe plantUML "@startuml" `shouldBe` Nothing
      it "@startuml and @enduml" $ P.parse plantUML "" "@startuml@enduml" `shouldBe` (Right (PlantUML []))
      it "@startuml and @enduml" $ P.parse plantUML "" "@startuml actor A @enduml"
        `shouldBe` (Right (PlantUML [SubjectDef (Actor (Name "A") Nothing Nothing Nothing)]))
      it "@startuml and @enduml" $ P.parse plantUML "" "@startuml actor A as a A -> B : aaa\n@enduml"
        `shouldBe` (Right (PlantUML [SubjectDef (Actor (Name "A") (Just (Alias "a")) Nothing Nothing),ArrowDef (Arrow2 (Just "A") (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just "B") (Just " aaa"))]))



