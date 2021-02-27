{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.PlantUML.ParserSpec where

import qualified Data.Text as T
import Text.Megaparsec 
    ( between, choice, MonadParsec(try), many, manyTill, notFollowedBy, lookAhead, ParseErrorBundle(..), Parsec(..))
import qualified Text.Megaparsec  as M (parse, parseMaybe)


import Text.Megaparsec.Char as C ( space1, string, printChar, char )
import Text.Megaparsec.Char.Lexer () 
import Language.PlantUML.Types

import Language.PlantUML.Parser
    ( plantUML,
      asName,
      declSubject,
      declArrow,
      color,
      reservedAs,
      declNotes,
      declGrouping,
      declCommand,
      name, 
      skinParamAssoc,
      skinParamParser,
      skinParametersParser,
      stereotype,
      arrow,
      linesTill',
      end
    )

import Test.Hspec ( describe, it, shouldBe, Spec )

import qualified Language.PlantUML.ParserHelper as P (
  parse,
  parseMaybe,
  assocParser,
  dropContinuationLine,
  lexeme,
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
    describe "name" $ do
      it "quoted"     $ P.parseMaybe name "\"ab\"" `shouldBe` (Just (Q "ab"))
      it "non quoted" $ P.parseMaybe name "ab" `shouldBe` (Just (Nq "ab"))

    describe "reservedAs" $ do
      it "use alias" $ P.parse reservedAs "" "as abc" `shouldBe` (Right (Nq "abc"))
      
      it "no alias" $ P.parseMaybe reservedAs "as" `shouldBe` Nothing
      it "works after alias" $ P.parse (P.lexeme reservedAs) "" "as abc " `shouldBe` (Right (Nq "abc"))

      
      
      it "works after alias" $ P.parse (P.lexeme reservedAs *> string "string") "" "as abc string" `shouldBe` (Right "string")
      it "use alias" $ P.parseMaybe reservedAs "" `shouldBe` Nothing

    describe "space + reservedAs" $ do
      it "use alias" $ P.parse (try $ P.spaceConsumer *> reservedAs) "" " as abc" `shouldBe` (Right  (Nq "abc"))
      it "no alias" $ P.parseMaybe (try $ P.spaceConsumer *> reservedAs) " as" `shouldBe` Nothing
      it "works after alias" $ P.parse (try $ P.spaceConsumer *> P.lexeme reservedAs) "" "  as abc " `shouldBe` (Right (Nq "abc"))
      it "works after alias" $ P.parse (try (P.spaceConsumer *> P.lexeme reservedAs) *> string "string") "" "  as abc string"
        `shouldBe` (Right "string")
      it "empty" $ P.parseMaybe (try (P.spaceConsumer *> reservedAs)) " "
        `shouldBe` Nothing
      it "Foo1" $ P.parse (reservedAs) "" "as Foo1"
        `shouldBe` (Right (Nq "Foo1"))


    describe "color" $ do
      it "red" $ P.parse color "" "#red"
        `shouldBe` (Right (Color Red))
      it "Hex" $ P.parse color "" "#992233"
        `shouldBe` (Right (HexColor "992233"))

    describe "declSubject" $ do
      it "participant w/o alias" $ P.parse declSubject "" "participant abc"
        `shouldBe` (Right (Subject Participant (Name1 (Nq "abc")) Nothing Nothing Nothing))
      it "participant w alias" $ P.parse declSubject "" ("participant abc as a")
        `shouldBe` (Right (Subject Participant (AliasedName (Nq "abc") (Nq "a")) Nothing Nothing Nothing))
      -- more variation
      it "actor w/o alias" $ P.parse declSubject "" "actor abc"
        `shouldBe` (Right (Subject Actor (Name1 (Nq "abc")) Nothing Nothing Nothing))
      it "boundary w/o alias" $ P.parse declSubject "" "boundary abc"
        `shouldBe` (Right (Subject Boundary (Name1 (Nq "abc")) Nothing Nothing Nothing))
      it "control w/o alias" $ P.parse declSubject "" "control abc"
        `shouldBe` (Right (Subject Control (Name1 (Nq "abc")) Nothing Nothing Nothing))
      it "entity w/o alias" $ P.parse declSubject "" "entity abc"
        `shouldBe` (Right (Subject Entity (Name1 (Nq "abc")) Nothing Nothing Nothing))
      it "database w/o alias" $ P.parse declSubject "" "database abc"
        `shouldBe` (Right (Subject Database (Name1 (Nq "abc")) Nothing Nothing Nothing))
      it "database w/o alias" $ P.parse declSubject "" "database abc"
        `shouldBe` (Right (Subject Database (Name1 (Nq "abc")) Nothing Nothing Nothing))
      it "collections w/o alias" $ P.parse declSubject "" "collections abc"
        `shouldBe` (Right (Subject Collections (Name1 (Nq "abc")) Nothing Nothing Nothing))
      it "queue w/o alias" $ P.parse declSubject "" "queue abc"
        `shouldBe` (Right (Subject Queue (Name1 (Nq "abc")) Nothing Nothing Nothing))
      it "consective actors" $ P.parse declSubject "" "participant participant as Foo \nactor actor as Foo1"
        `shouldBe` (Right (Subject Participant (AliasedName (Nq "participant") (Nq "Foo")) Nothing Nothing Nothing))
        
      it "actor with order" $ P.parse declSubject "" "actor A order 10"
        `shouldBe` (Right (Subject Actor (Name1 (Nq "A")) Nothing (Just 10) Nothing))
      it "actor with color" $ P.parse declSubject "" "actor A #red"
        `shouldBe` (Right (Subject Actor (Name1 (Nq "A")) Nothing Nothing (Just (Color Red))))
      it "actor with color and order" $ P.parse declSubject "" "actor A order 10 #red"
        `shouldBe` (Right (Subject Actor (Name1 (Nq "A")) Nothing (Just 10) (Just (Color Red))))
      it "actor with alias, color and color" $ P.parse declSubject "" "actor A as Foo2 order 10 #red"
        `shouldBe` (Right (Subject Actor (AliasedName (Nq "A")  (Nq "Foo2")) Nothing (Just 10) (Just (Color Red))))
      it "participant with stereotype " $ P.parse declSubject "" "participant Bob << (C,#ADD1B2) >>\n" -- 
        `shouldBe` (Right (Subject Participant (Name1 (Nq "Bob")) (Just (Stereotype " (C,#ADD1B2) ")) Nothing Nothing))
{-
    describe "(manyTill printChar rightEnd)" $ do
      it "manyTill:" $ P.parse (manyTill printChar rightEnd) "" "first >> "
        `shouldBe` (Right "one line>>")
      it "manyTill:" $ P.parse (manyTill printChar rightEnd) "" "first >>> second> a> >>"
        `shouldBe` (Right "abc ")
      it "manyTill:" $ P.parse (manyTill printChar rightEnd) "" "what > is >> this >>>"
        `shouldBe` (Right "abc ")
-}

    describe "stereotype" $ do
      it "no >>" $ P.parseMaybe stereotype  "<<first\n"
        `shouldBe` Nothing
      it "no >>" $ P.parseMaybe stereotype  "<<first>\n"
        `shouldBe` Nothing
      
      it "one >>" $ P.parse stereotype "" "<<first>>\n"
        `shouldBe` (Right (Stereotype "first"))
      it "one >>" $ P.parse stereotype "" "<<first>> \n"
        `shouldBe` (Right (Stereotype "first"))
      it "one >>" $ P.parse stereotype "" "<<first>>>\n"
        `shouldBe` (Right (Stereotype "first>"))
      it "one >>" $ P.parse stereotype "" "<<first>>> \n"
        `shouldBe` (Right (Stereotype "first>"))
        
      it "one >>" $ P.parse stereotype "" "<<first> >>>\n"
        `shouldBe` (Right (Stereotype "first> >"))
      it "one >>" $ P.parse stereotype "" "<<first> >>> \n"
        `shouldBe` (Right (Stereotype "first> >"))
      it "two >>" $ P.parse stereotype "" "<<first> >>> second>>\n"
        `shouldBe` (Right (Stereotype "first> >>> second"))
      it "two >>" $ P.parse stereotype "" "<<first> >>> second>> \n"
        `shouldBe` (Right (Stereotype "first> >>> second"))
      it "two >>" $ P.parse stereotype "" "<<first> >>> second>>>\n"
        `shouldBe` (Right (Stereotype "first> >>> second>"))

      

--    describe "arrow1" $ do
--      it "arrow" $ parse (choice parrows) "" "->" `shouldBe` (Right "->")

    describe "arrow" $ do
      it "A -> B : a b c" $ P.parse declArrow "" "A -> B :a b c\n "
        `shouldBe` (Right (Arrow (Just (Nq "A")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">"))
                           (Just (Name1 (Nq "B"))) (Just "a b c")))
      it "A->B" $ P.parse declArrow "" "A->B "
        `shouldBe` (Right (Arrow (Just (Nq "A")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "B"))) Nothing))
      it "-> B" $ P.parse declArrow "" "-> B "
        `shouldBe` (Right (Arrow Nothing (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "B"))) Nothing))
      it "-> B" $ P.parse declArrow "" "-> B : a b c\n"
        `shouldBe` (Right (Arrow Nothing (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "B"))) (Just " a b c")))
      it "A->" $ P.parse declArrow "" "A -> : a b c\n"
        `shouldBe` (Right (Arrow (Just (Nq "A")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) Nothing (Just " a b c")))
      it "Bob()" $ P.parse declArrow "" "Alice -> \"Bob()\" : Hello\n"
        `shouldBe` (Right (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">"))
                            (Just (Name1 (Q "Bob()"))) (Just " Hello")))
      it "Long" $ P.parse declArrow "" "\"Bob()\" -> Long as \"This is very\\\nlong\"\n"
        `shouldBe` (Right (Arrow (Just (Q "Bob()")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (AliasedName (Nq "Long") (Q "This is verylong"))) Nothing))
      it "Bob()2" $ P.parse declArrow "" "Long --> \"Bob()\" : ok\n"
        `shouldBe` (Right (Arrow (Just (Nq "Long")) (Arr Nothing (Shaft (Just "--") Nothing Nothing) (Just ">")) (Just (Name1 (Q "Bob()"))) (Just " ok")))
      it "A -> \"B\" as b" $ P.parse declArrow "" "A -> \"B\" as b : message !\n"
        `shouldBe`
        (Right (Arrow (Just (Nq "A"))
                        (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (AliasedName (Q "B") (Nq "b")))
                        (Just " message !")))
      it "bill -> bob #005500" $ do
        P.parse declArrow "" "bill -> bob #005500\n"
        `shouldBe` (Right (Arrow2 (Just (Nq "bill")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "bob"))) (HexColor "005500") Nothing))
        
      it "bill -> bob #005500 : hello from thread 2" $ do
        P.parse declArrow "" "bill -> bob #005500 : hello from thread 2\n"
        `shouldBe` (Right (Arrow2 (Just (Nq "bill")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "bob"))) (HexColor "005500") (Just " hello from thread 2")))
      it "bill -> bob #red : hello from thread 2" $ do
        P.parse declArrow "" "bill -> bob #red : hello from thread 2\n"
        `shouldBe` (Right (Arrow2 (Just (Nq "bill")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "bob"))) (Color Red) (Just " hello from thread 2")))

      it "bob -> george ** : create" $ do
        P.parse declArrow "" "bob -> george ** : create\n"
        `shouldBe` (Right (ActivationArrow (Just (Nq "bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Nq "george") Creation (Just " create")))


    describe "return only" $ do
      it "return" $ P.parse declArrow "" "return\n" `shouldBe` (Right (Return Nothing))
    describe "return with space only" $ do
      it "return" $ P.parse declArrow "" "return  \n" `shouldBe` (Right (Return Nothing))
    describe "return with message" $ do
      it "return" $ P.parse declArrow "" "return   statement\n" `shouldBe` (Right (Return (Just "statement")))

    describe "arrow color" $ do
      it "->" $ P.parse arrow "" "->" `shouldBe` (Right (PreArr Nothing Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")Nothing ))
      it "<-" $ P.parse arrow "" "<-" `shouldBe` (Right (PreArr Nothing (Just "<") (Shaft (Just "-") Nothing Nothing) Nothing Nothing))
      it "->" $ P.parse arrow "" "<->" `shouldBe` (Right (PreArr Nothing (Just "<") (Shaft (Just "-") Nothing Nothing) (Just ">") Nothing))
      it "-->" $ P.parse arrow "" "-->" `shouldBe` (Right (PreArr Nothing Nothing (Shaft (Just "--") Nothing Nothing) (Just ">") Nothing))
      it "->]" $ P.parse arrow "" "->]" `shouldBe` (Right (PreArr Nothing Nothing (Shaft (Just "-") Nothing Nothing) (Just ">") (Just "]")))
      it "->0]" $ P.parse arrow "" "->o]" `shouldBe` (Right (PreArr Nothing Nothing (Shaft (Just "-") Nothing Nothing) (Just ">o") (Just "]")))
      it "[->" $ P.parse arrow "" "[->" `shouldBe` (Right (PreArr (Just "[") Nothing (Shaft (Just "-") Nothing Nothing) (Just ">") Nothing))
      it "[->" $ P.parse arrow "" "[o->" `shouldBe` (Right (PreArr (Just "[") (Just "o")  (Shaft (Just "-") Nothing Nothing) (Just ">") Nothing))

      it "[#red]-->" $ P.parseMaybe arrow "[#red]-->"
        `shouldBe` Nothing
      it "-[#red]->" $ P.parse arrow "" "-[#red]->"
        `shouldBe` (Right (PreArr Nothing Nothing (Shaft (Just "-") (Just (Color Red)) (Just "-")) (Just ">") Nothing))
      it "--[#red]>" $ P.parse arrow "" "--[#red]>"
        `shouldBe` (Right (PreArr Nothing Nothing (Shaft (Just "--") (Just (Color Red)) Nothing) (Just ">") Nothing))
      it "o--[#red]>" $ P.parse arrow "" "o--[#red]>x"
        `shouldBe` (Right (PreArr Nothing (Just "o") (Shaft (Just "--") (Just (Color Red)) Nothing) (Just ">x") Nothing))
      it "--->" $ P.parse arrow "" "--->" `shouldBe` (Right (PreArr Nothing Nothing (Shaft (Just "---") Nothing Nothing) (Just ">")Nothing))
      it "<--->" $ P.parse arrow "" "<--->" `shouldBe` (Right (PreArr Nothing (Just "<") (Shaft (Just "---") Nothing Nothing) (Just ">") Nothing))



    describe "note left of" $ do
      it "note left of A" $ P.parse declNotes "" ("note left of A\nthis\nend note\n")
        `shouldBe` (Right (NoteLeft Note (Just (Nq "A"))  Nothing ["this"]))
      it "note left of #red" $ P.parse declNotes "" ("note left of #red\nthis\nend note\n")
        `shouldBe` (Right (NoteLeft Note Nothing (Just (Color Red)) ["this"]))
      it "note left of A #red" $ P.parse declNotes "" ("note left of A #red\nthis\nend note\n")
        `shouldBe` (Right (NoteLeft Note (Just (Nq "A")) (Just (Color Red)) ["this"]))
      it "note left of" $ P.parse declNotes "" ("note left of \nthis\nend note\n")
        `shouldBe` (Right (NoteLeft Note Nothing Nothing ["this"]))
    describe "note left :" $ do
      it "note left A :" $ P.parse declNotes "" ("note left A : this\n")
        `shouldBe` (Right (NoteLeft Note (Just (Nq "A")) Nothing [" this"]))
      it "note left #red :" $ P.parse declNotes "" ("note left #red : this\n")
        `shouldBe` (Right (NoteLeft Note Nothing (Just (Color Red)) [" this"]))
      it "note left A #red :" $ P.parse declNotes "" ("note left A #red: this\n")
        `shouldBe` (Right (NoteLeft Note (Just (Nq "A")) (Just (Color Red)) [" this"]))
      it "note left" $ P.parse declNotes "" ("note left \nthis\nend note\n")
        `shouldBe` (Right (NoteLeft Note Nothing Nothing ["", "this"]))

    describe "multi line note" $ do
      it "note left multi" $ P.parse declNotes "" ("note left\nright1\nright2\nend note\n")
        `shouldBe` (Right (NoteLeft Note Nothing Nothing ["", "right1", "right2"]))

      it "note left multi" $ P.parse declNotes "" ("note left \nright1\nend note\n")
        `shouldBe` (Right (NoteLeft Note Nothing Nothing ["", "right1"]))

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
        `shouldBe`  (Right (Grouping Group "A" [[ArrowDef (Arrow (Just (Nq "A")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "B"))) Nothing)]]))
      it "alt else" $ P.parse declGrouping "" "alt a\nA->B: A -> B\nelse\nB->C: B-> C\nend alt\n"
        `shouldBe` ( Right (Grouping Alt "a" [[ArrowDef (Arrow (Just (Nq "A")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "B"))) (Just " A -> B"))],[ArrowDef (Arrow (Just (Nq "B")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "C"))) (Just " B-> C"))]]))
      it "opt else else" $ P.parse declGrouping "" "opt a\nA->B: A -> B\nelse\nB->C: B-> C\nelse abc\nC->D: C -> D\nend opt\n"
        `shouldBe` (Right (Grouping Opt "a" [[ArrowDef (Arrow (Just (Nq "A")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "B"))) (Just " A -> B"))],[ArrowDef (Arrow (Just (Nq "B")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "C"))) (Just " B-> C"))],[ArrowDef (Arrow (Just (Nq "C")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "D"))) (Just " C -> D"))]]))

    describe "command" $ do
      it "autonumber" $ P.parse declCommand "" "autonumber\n" `shouldBe` (Right (Autonumber (Start Nothing Nothing Nothing)))
      it "autonumber 10" $ P.parse declCommand "" "autonumber 10"
        `shouldBe` (Right (Autonumber (Start (Just 10) Nothing Nothing)))      
      it "autonumber 10 20" $ P.parse declCommand "" "autonumber 10 20"
        `shouldBe` (Right (Autonumber (Start (Just 10) (Just 20) ( Nothing))))
                          
      it "autonumber 10 20 \"30\"" $ P.parse declCommand "" "autonumber 10 20 \"30\""
        `shouldBe` (Right (Autonumber (Start (Just 10) (Just 20) (Just "30"))))
      it "autonumber stop" $ P.parse declCommand "" "autonumber stop"
        `shouldBe` (Right (Autonumber Stop))
      it "autonumber resume 1" $ P.parse declCommand "" "autonumber resume"
        `shouldBe` (Right (Autonumber (Resume Nothing Nothing)))
      it "autonumber resume" $ P.parse declCommand "" "autonumber resume 10"
        `shouldBe` (Right (Autonumber (Resume (Just 10) Nothing)))
      it "autonumber resume 3" $ P.parse declCommand "" "autonumber resume 10 \"ABC\""
        `shouldBe` (Right (Autonumber (Resume (Just 10) (Just "ABC"))))
        
      it "autoactivate On" $ P.parse declCommand "" "autoactivate on" `shouldBe` (Right (AutoActivate On))
      it "autoactivate Off" $ P.parse declCommand "" "autoactivate off" `shouldBe` (Right (AutoActivate Off))
      it "activate" $ P.parse declCommand "" "activate A" `shouldBe` (Right (Activate (Nq "A") Nothing))
      it "deactivate" $ P.parse declCommand "" "deactivate B" `shouldBe` (Right (Deactivate (Nq "B")))
      it "activate" $ P.parse declCommand "" "activate A #DarkSalmon" `shouldBe` (Right (Activate (Nq "A") (Just (Color DarkSalmon))))

      it "title" $ P.parse declCommand "" "title A\n" `shouldBe` (Right (Title " A"))
      it "title" $ P.parse declCommand "" "title A\\a\n" `shouldBe` (Right (Title " A\\a"))
      it "title" $ P.parseMaybe declCommand "title A" `shouldBe` Nothing
      it "multiple line title" $ P.parse declCommand "" "title\nab c\nend title\n" `shouldBe` (Right (Title "ab c"))
      it "multiple line title" $ P.parse declCommand "" "title \na\nb\nend title\n" `shouldBe` (Right (Title " ab"))
      
      it "newpage" $ P.parse declCommand "" "newpage\n" `shouldBe` (Right (NewPage Nothing))
      it "newpage" $ P.parse declCommand "" "newpage this is new\n" `shouldBe` (Right (NewPage (Just "this is new")))
      it "header" $ P.parse declCommand "" "header\n" `shouldBe` (Right (Header Nothing))
      it "header" $ P.parse declCommand "" "header newpage this is new\n" `shouldBe` (Right (Header (Just "newpage this is new")))
      it "header page1     header page2" $ P.parse (many declCommand) "" "header page1\n    header page2\n" `shouldBe` (Right [Header (Just "page1"),Header (Just "page2")])

      it "create name" $ P.parse declCommand "" ("create name\n")
        `shouldBe` (Right (LifeLine Create Nothing (Nq "name")))
      it "create control name" $ P.parse declCommand "" ("create control name\n")
        `shouldBe` (Right (LifeLine Create (Just Control) (Nq "name")))
      it "divider normal" $ P.parse declCommand "" ("== normal ==\n")
        `shouldBe` (Right (Divider " normal "))
      it "divider ==" $ P.parse declCommand "" ("=== === = ===\n")
        `shouldBe` (Right (Divider " === = ="))
        
      it "space |||" $ P.parse declCommand "" ("|||\n")
        `shouldBe` (Right (Space Nothing))
      it "space |||" $ P.parse declCommand "" ("||||\n")
        `shouldBe` (Right (Space Nothing))
      it "space |||| " $ P.parse declCommand "" ("|||| \n")
        `shouldBe` (Right (Space Nothing))
      it "space ||40|" $ P.parse declCommand "" ("||40|\n")
        `shouldBe` (Right (Space (Just 40)))
      it "space ||50||" $ P.parse declCommand "" ("||50||\n")
        `shouldBe` (Right (Space (Just 50)))
      it "space |||60| " $ P.parse declCommand "" ("|||60| \n")
        `shouldBe` (Right (Space (Just 60)))        

        
    describe "end" $ do
      it "end title" $
        P.parse (end "title") "" "end title"
        `shouldBe` (Right ())
        
    describe "linesTill" $ do
      it "end title" $
        P.parse (linesTill' "title" []) "" "\nend title\n"
        `shouldBe` (Right [""])


    describe "skin parameters" $ do
      it "responseMessageBelowArrow" $
        P.parse (P.assocParser skinParamAssoc) "" "responseMessageBelowArrow true"
        `shouldBe` (Right (ResponseMessageBelowArrow True))
        
      it "skinparam responseMessageBelowArrow true" $
        P.parse skinParamParser "" "skinparam responseMessageBelowArrow true"
        `shouldBe` (Right (SkinParameters [ResponseMessageBelowArrow True]))
      it "skinparam responseMessageBelowArrow false" $
        P.parse skinParamParser "" "skinparam responseMessageBelowArrow false"
        `shouldBe` (Right (SkinParameters [ResponseMessageBelowArrow False]))
      it "skinparam maxMessageSize" $
        P.parse skinParamParser "" "skinparam maxMessageSize 10"
        `shouldBe` (Right (SkinParameters [MaxMessageSize 10]))

    describe "skinparam sequence" $ do
      it "empty" $
        P.parse skinParametersParser "" "skinparam sequence {}"
        `shouldBe` (Right (SkinParameters []))
      it "one entry" $
        P.parse skinParametersParser "" "skinparam sequence {maxMessageSize 20}"
        `shouldBe` (Right (SkinParameters [MaxMessageSize 20]))
      it "one entry" $
        P.parse skinParametersParser "" "skinparam sequence\n{\nmaxMessageSize 20\n}"
        `shouldBe` (Right (SkinParameters [MaxMessageSize 20]))
      it "two entries" $
        P.parse skinParametersParser "" "skinparam  sequence \n { \nmaxMessageSize 20 \n maxMessageSize 10\n } \n"
        `shouldBe` (Right (SkinParameters [MaxMessageSize 20, MaxMessageSize 10]))
      it "ParticipantBorderColor DeepSkyBlue, MaxMessageSize 10" $
        P.parse skinParametersParser "" "skinparam  sequence \n { \nParticipantBorderColor DeepSkyBlue\n MaxMessageSize 10\n} \n"
        `shouldBe` (Right (SkinParameters [ParticipantBorderColor (Color DeepSkyBlue), MaxMessageSize 10]))


    describe "asName" $ do
      it "name only" $ P.parse asName "" "name" `shouldBe` (Right (Name1 (Nq "name")))
      it "quoted name only" $ P.parse asName "" "\"name\"" `shouldBe` (Right (Name1 (Q "name")))
      it "name with alias" $ P.parse asName "" "name as n" `shouldBe` (Right (AliasedName (Nq "name") (Nq "n")))
      it "quoted name only" $ P.parse asName "" "\"name\"" `shouldBe` (Right (Name1 (Q "name")))
      it "quoted name with alias" $ P.parse asName "" "\"name\" as n " `shouldBe` (Right (AliasedName (Q "name") (Nq "n")))
      



    describe "uml" $ do
      it "@startuml" $ P.parseMaybe plantUML "@startuml" `shouldBe` Nothing
      it "@startuml and @enduml" $ P.parse plantUML "" "@startuml@enduml" `shouldBe` (Right (PlantUML []))
      it "@startuml and @enduml" $ P.parse plantUML "" "@startuml actor A @enduml"
        `shouldBe` (Right (PlantUML [SubjectDef (Subject Actor (Name1 (Nq "A")) Nothing Nothing Nothing)]))
      it "@startuml and @enduml" $ P.parse plantUML "" "@startuml actor A as a A -> B : aaa\n@enduml"
        `shouldBe` (Right (PlantUML [SubjectDef (Subject Actor (AliasedName (Nq "A") (Nq "a")) Nothing Nothing Nothing),
                                     ArrowDef (Arrow (Just (Nq "A")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "B"))) (Just " aaa"))]))


{-
rightEnd :: MonadParsec Char T.Text m => m T.Text
rightEnd = do
      string ">>"
      rs <- many (char '>')
      return $ T.append ">>" (T.pack rs)
-}


