{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.PlantUMLSpec where
{- Test cases in this files are from PlantUML web page:
   https://plantuml.com/sequence-diagram
-}

import NeatInterpolation
import qualified Data.Text as T

import Language.PlantUML

import Test.Hspec

spec :: Spec
spec = do
  describe "silly" $ do
    it "test" $ 1 `shouldBe` 1
  describe "plantuml sequence diagram" $ do
    it "Basic example" $ do
      let input :: T.Text    
          input = [text|
@startuml
Alice -> Bob: Authentication Request
Bob --> Alice: Authentication Response

Alice -> Bob: Another authentication Request
Alice <-- Bob: Another authentication Response
@enduml
|]
      parsePlantUML input `shouldBe` (Right (PlantUML [ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing (Just " Authentication Request")),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "--") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " Authentication Response")),ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing (Just " Another authentication Request")),ArrowDef (Arrow (Just (Nq "Alice")) (Arr (Just "<") (Shaft (Just "--") Nothing Nothing) Nothing) (Just (Name1 (Nq "Bob"))) Nothing (Just " Another authentication Response"))]))

    it "Declaring participant" $ do
      let input :: T.Text
          input = [text|
@startuml
participant Participant as Foo
actor       actor       as Foo1
boundary    boundary    as Foo2
control     control     as Foo3
entity      entity      as Foo4
database    database    as Foo5
collections collections as Foo6
queue       queue       as Foo7
Foo -> Foo1 : To actor 
Foo -> Foo2 : To boundary
Foo -> Foo3 : To control
Foo -> Foo4 : To entity
Foo -> Foo5 : To database
Foo -> Foo6 : To collections
Foo -> Foo7: To queue
@enduml
|]
      parsePlantUML input `shouldBe`
        (Right (PlantUML
                [SubjectDef (Subject Participant (AliasedName (Nq "Participant") (Nq "Foo")) Nothing Nothing Nothing),
                  SubjectDef (Subject Actor (AliasedName (Nq "actor") (Nq "Foo1")) Nothing Nothing Nothing),
                  SubjectDef (Subject Boundary (AliasedName (Nq "boundary") (Nq "Foo2")) Nothing Nothing Nothing),
                  SubjectDef (Subject Control (AliasedName (Nq "control") (Nq "Foo3")) Nothing Nothing Nothing),
                  SubjectDef (Subject Entity (AliasedName (Nq "entity") (Nq "Foo4")) Nothing Nothing Nothing),
                  SubjectDef (Subject Database (AliasedName (Nq "database") (Nq "Foo5")) Nothing Nothing Nothing),
                  SubjectDef (Subject Collections (AliasedName (Nq "collections") (Nq "Foo6")) Nothing Nothing Nothing),
                  SubjectDef (Subject Queue (AliasedName (Nq "queue") (Nq "Foo7")) Nothing Nothing Nothing),
                  ArrowDef (Arrow (Just (Nq "Foo")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Foo1"))) Nothing (Just " To actor ")),ArrowDef (Arrow (Just (Nq "Foo")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Foo2"))) Nothing (Just " To boundary")),ArrowDef (Arrow (Just (Nq "Foo")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Foo3"))) Nothing (Just " To control")),ArrowDef (Arrow (Just (Nq "Foo")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Foo4"))) Nothing (Just " To entity")),ArrowDef (Arrow (Just (Nq "Foo")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Foo5"))) Nothing (Just " To database")),ArrowDef (Arrow (Just (Nq "Foo")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Foo6"))) Nothing (Just " To collections")),ArrowDef (Arrow (Just (Nq "Foo")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Foo7"))) Nothing (Just " To queue"))]))

    it "Declaring participants 2" $ do
      let input :: T.Text
          input = [text|
@startuml
actor Bob #red
' The only difference between actor
'and participant is the drawing
participant Alice
participant "I have a really\nlong name" as L #99FF99
/' You can also declare:
   participant L as "I have a really\nlong name"  #99FF99
  '/

Alice->Bob: Authentication Request
Bob->Alice: Authentication Response
Bob->L: Log transaction
@enduml
|]
      parsePlantUML input `shouldBe`
        (Right (PlantUML [
                   SubjectDef (Subject Actor (Name1 (Nq "Bob")) Nothing Nothing (Just (Color Red))),
                   SubjectDef (Subject Participant (Name1 (Nq "Alice")) Nothing Nothing Nothing),
                   SubjectDef (Subject Participant (AliasedName (Q "I have a really\\nlong name") (Nq "L")) Nothing Nothing (Just (HexColor "99FF99"))),
                   ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing (Just " Authentication Request")),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " Authentication Response")),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "L"))) Nothing (Just " Log transaction"))]))

    it "Declaring participants 3" $ do
      let input :: T.Text
          input = [text|
@startuml
participant Last order 30
participant Middle order 20
participant First order 10
@enduml
|]
      parsePlantUML input `shouldBe`  (Right
                                       (PlantUML
                                        [SubjectDef (Subject Participant (Name1 (Nq "Last")) Nothing (Just 30)  Nothing),
                                         SubjectDef (Subject Participant (Name1 (Nq "Middle")) Nothing (Just 20) Nothing),
                                         SubjectDef (Subject Participant (Name1 (Nq "First")) Nothing (Just 10) Nothing)]))



    it "Use non-letters in participants" $ do
      let input :: T.Text
          input = [text|
@startuml
Alice -> "Bob()" : Hello
"Bob()" -> "This is very\nlong" as Long
' You can also declare:
' "Bob()" -> Long as "This is very\nlong"
Long --> "Bob()" : ok
@enduml
|]
      parsePlantUML input `shouldBe` (Right (PlantUML [ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Q "Bob()"))) Nothing (Just " Hello")),ArrowDef (Arrow (Just (Q "Bob()")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (AliasedName (Q "This is very\\nlong") (Nq "Long"))) Nothing Nothing),ArrowDef (Arrow (Just (Nq "Long")) (Arr Nothing (Shaft (Just "--") Nothing Nothing) (Just ">")) (Just (Name1 (Q "Bob()"))) Nothing (Just " ok"))]))
    it "Message to self" $ do
      let input :: T.Text
          input = [text|
@startuml
Alice -> Alice: This is a signal to self.\nIt also demonstrates\nmultiline \ntext
@enduml
|]
      parsePlantUML input `shouldBe` (Right (PlantUML [ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " This is a signal to self.\\nIt also demonstrates\\nmultiline \\ntext"))]))


    it "Text alignment" $ do
      let input :: T.Text
          input = [text|
@startuml
skinparam responseMessageBelowArrow true
Bob -> Alice : hello
Alice -> Bob : ok
@enduml
|]
      parsePlantUML input `shouldBe` (Right (PlantUML [CommandDef (SkinParameters [ResponseMessageBelowArrow True]),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " hello")),ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing (Just " ok"))]))

      
    it "Change arrow style" $ do
      let input :: T.Text
          input = [text|
@startuml
Bob ->x Alice
Bob -> Alice
Bob ->> Alice
Bob -\ Alice
Bob \\- Alice
Bob //-- Alice

Bob ->o Alice
Bob o\\-- Alice

Bob <-> Alice
Bob <->o Alice
@enduml
|]
      parsePlantUML input `shouldBe` (Right (PlantUML [ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">x")) (Just (Name1 (Nq "Alice"))) Nothing Nothing),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing Nothing),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">>")) (Just (Name1 (Nq "Alice"))) Nothing Nothing),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just "\\")) (Just (Name1 (Nq "Alice"))) Nothing Nothing),ArrowDef (Arrow (Just (Nq "Bob")) (Arr (Just "\\") (Shaft (Just "-") Nothing Nothing) Nothing) (Just (Name1 (Nq "Alice"))) Nothing Nothing),ArrowDef (Arrow (Just (Nq "Bob")) (Arr (Just "//") (Shaft (Just "--") Nothing Nothing) Nothing) (Just (Name1 (Nq "Alice"))) Nothing Nothing),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">o")) (Just (Name1 (Nq "Alice"))) Nothing Nothing),ArrowDef (Arrow (Just (Nq "Bob")) (Arr (Just "o\\") (Shaft (Just "--") Nothing Nothing) Nothing) (Just (Name1 (Nq "Alice"))) Nothing Nothing),ArrowDef (Arrow (Just (Nq "Bob")) (Arr (Just "<") (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing Nothing),ArrowDef (Arrow (Just (Nq "Bob")) (Arr (Just "<") (Shaft (Just "-") Nothing Nothing) (Just ">o")) (Just (Name1 (Nq "Alice"))) Nothing Nothing)]))


    it "Change arrow color" $ do
      let input :: T.Text
          input = [text|
@startuml
Bob -[#red]> Alice : hello
Alice -[#0000FF]->Bob : ok
@enduml
|]
      parsePlantUML input `shouldBe` (Right (PlantUML [ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") (Just (Color Red)) Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " hello")),ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") (Just (HexColor "0000FF")) (Just "-")) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing (Just " ok"))]))

    it "Message sequence number 1" $ do
      let input :: T.Text
          input = [text|
@startuml
autonumber
Bob -> Alice : Authentication Request
Bob <- Alice : Authentication Response
@enduml
|]
      parsePlantUML input `shouldBe` (Right (PlantUML [CommandDef (Autonumber (Start Nothing Nothing Nothing)),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " Authentication Request")),ArrowDef (Arrow (Just (Nq "Bob")) (Arr (Just "<") (Shaft (Just "-") Nothing Nothing) Nothing) (Just (Name1 (Nq "Alice"))) Nothing (Just " Authentication Response"))])) 



    it "Message sequence number 2" $ do
      let input :: T.Text
          input = [text|
@startuml
autonumber
Bob -> Alice : Authentication Request
Bob <- Alice : Authentication Response

autonumber 15
Bob -> Alice : Another authentication Request
Bob <- Alice : Another authentication Response

autonumber 40 10
Bob -> Alice : Yet another authentication Request
Bob <- Alice : Yet another authentication Response

@enduml
|]
      parsePlantUML input `shouldBe` ( Right (PlantUML [CommandDef (Autonumber (Start Nothing Nothing Nothing)),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " Authentication Request")),ArrowDef (Arrow (Just (Nq "Bob")) (Arr (Just "<") (Shaft (Just "-") Nothing Nothing) Nothing) (Just (Name1 (Nq "Alice"))) Nothing (Just " Authentication Response")),CommandDef (Autonumber (Start (Just 15) Nothing Nothing)),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " Another authentication Request")),ArrowDef (Arrow (Just (Nq "Bob")) (Arr (Just "<") (Shaft (Just "-") Nothing Nothing) Nothing) (Just (Name1 (Nq "Alice"))) Nothing (Just " Another authentication Response")),CommandDef (Autonumber (Start (Just 40) (Just 10) Nothing)),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " Yet another authentication Request")),ArrowDef (Arrow (Just (Nq "Bob")) (Arr (Just "<") (Shaft (Just "-") Nothing Nothing) Nothing) (Just (Name1 (Nq "Alice"))) Nothing (Just " Yet another authentication Response"))]))

                                     

        
    it "Message sequence number 3" $ do
      let input :: T.Text
          input = [text|
@startuml
autonumber "<b>[000]"
Bob -> Alice : Authentication Request
Bob <- Alice : Authentication Response

autonumber 15 "<b>(<u>##</u>)"
Bob -> Alice : Another authentication Request
Bob <- Alice : Another authentication Response

autonumber 40 10 "<font color=red><b>Message 0  "
Bob -> Alice : Yet another authentication Request
Bob <- Alice : Yet another authentication Response

@enduml|]
      parsePlantUML input `shouldBe` (Right (PlantUML [CommandDef (Autonumber (Start Nothing Nothing (Just "<b>[000]"))),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " Authentication Request")),ArrowDef (Arrow (Just (Nq "Bob")) (Arr (Just "<") (Shaft (Just "-") Nothing Nothing) Nothing) (Just (Name1 (Nq "Alice"))) Nothing (Just " Authentication Response")),CommandDef (Autonumber (Start (Just 15) Nothing (Just "<b>(<u>##</u>)"))),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " Another authentication Request")),ArrowDef (Arrow (Just (Nq "Bob")) (Arr (Just "<") (Shaft (Just "-") Nothing Nothing) Nothing) (Just (Name1 (Nq "Alice"))) Nothing (Just " Another authentication Response")),CommandDef (Autonumber (Start (Just 40) (Just 10) (Just "<font color=red><b>Message 0  "))),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " Yet another authentication Request")),ArrowDef (Arrow (Just (Nq "Bob")) (Arr (Just "<") (Shaft (Just "-") Nothing Nothing) Nothing) (Just (Name1 (Nq "Alice"))) Nothing (Just " Yet another authentication Response"))]))
        
    it "Message sequence number 4" $ do
      let input :: T.Text
          input = [text|
@startuml
autonumber 10 10 "<b>[000]"
Bob -> Alice : Authentication Request
Bob <- Alice : Authentication Response

autonumber stop
Bob -> Alice : dummy

autonumber resume "<font color=red><b>Message 0  "
Bob -> Alice : Yet another authentication Request
Bob <- Alice : Yet another authentication Response

autonumber stop
Bob -> Alice : dummy

autonumber resume 1 "<font color=blue><b>Message 0  "
Bob -> Alice : Yet another authentication Request
Bob <- Alice : Yet another authentication Response
@enduml|]
      parsePlantUML input `shouldBe` (Right (PlantUML [CommandDef (Autonumber (Start (Just 10) (Just 10) (Just "<b>[000]"))),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " Authentication Request")),ArrowDef (Arrow (Just (Nq "Bob")) (Arr (Just "<") (Shaft (Just "-") Nothing Nothing) Nothing) (Just (Name1 (Nq "Alice"))) Nothing (Just " Authentication Response")),CommandDef (Autonumber Stop),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " dummy")),CommandDef (Autonumber (Resume Nothing (Just "<font color=red><b>Message 0  "))),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " Yet another authentication Request")),ArrowDef (Arrow (Just (Nq "Bob")) (Arr (Just "<") (Shaft (Just "-") Nothing Nothing) Nothing) (Just (Name1 (Nq "Alice"))) Nothing (Just " Yet another authentication Response")),CommandDef (Autonumber Stop),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " dummy")),CommandDef (Autonumber (Resume (Just 1) (Just "<font color=blue><b>Message 0  "))),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " Yet another authentication Request")),ArrowDef (Arrow (Just (Nq "Bob")) (Arr (Just "<") (Shaft (Just "-") Nothing Nothing) Nothing) (Just (Name1 (Nq "Alice"))) Nothing (Just " Yet another authentication Response"))]))
        
    it "Page Title, Header and Footer" $ do
      let input :: T.Text
          input = [text|
@startuml

header Page Header
footer Page %page% of %lastpage%

title Example Title

Alice -> Bob : message 1
Alice -> Bob : message 2

@enduml|]
      parsePlantUML input `shouldBe` (Right (PlantUML [CommandDef (Header (Just "Page Header")),CommandDef (Footer (Just "Page %page% of %lastpage%")),CommandDef (Title " Example Title"),ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing (Just " message 1")),ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing (Just " message 2"))])) 

    it "Splitting diagrams" $ do
      let input :: T.Text
          input = [text|
@startuml

Alice -> Bob : message 1
Alice -> Bob : message 2

newpage

Alice -> Bob : message 3
Alice -> Bob : message 4

newpage A title for the\nlast page

Alice -> Bob : message 5
Alice -> Bob : message 6
@enduml|]
      parsePlantUML input `shouldBe` (Right (PlantUML [ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing (Just " message 1")),ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing (Just " message 2")),CommandDef (NewPage (Just "Alice -> Bob : message 3")),ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing (Just " message 4")),CommandDef (NewPage (Just "A title for the\\nlast page")),ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing (Just " message 5")),ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing (Just " message 6"))]))
        
    it "Grouping Message" $ do
      let input :: T.Text
          input = [text|
@startuml
Alice -> Bob: Authentication Request

alt successful case

    Bob -> Alice: Authentication Accepted

else some kind of failure

    Bob -> Alice: Authentication Failure
    group My own label
    Alice -> Log : Log attack start
        loop 1000 times
            Alice -> Bob: DNS Attack
        end
    Alice -> Log : Log attack end
    end

else Another type of failure

   Bob -> Alice: Please repeat

end
@enduml
|]
      parsePlantUML input `shouldBe` (Right (PlantUML [ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing (Just " Authentication Request")),GroupingDef (Grouping Alt "successful case" [[ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " Authentication Accepted"))],[ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " Authentication Failure")),GroupingDef (Grouping Group "My own label" [[ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Log"))) Nothing (Just " Log attack start")),GroupingDef (Grouping Loop "1000 times" [[ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing (Just " DNS Attack"))]]),ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Log"))) Nothing (Just " Log attack end"))]])],[ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " Please repeat"))]])]))
        
    it "Secondary group label" $ do
      let input :: T.Text
          input = [text|
@startuml
Alice->Bob : hello
note left: this is a first note

Bob->Alice : ok
note right: this is another note

Bob->Bob : I am thinking
note left
a note
can also be defined
on several lines
end note
@enduml
|]
      parsePlantUML input `shouldBe` (Right (PlantUML [ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing (Just " hello")),NotesDef (NoteLeft Note Nothing Nothing [" this is a first note"]),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " ok")),NotesDef (NoteRight Note Nothing Nothing [" this is another note"]),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing (Just " I am thinking")),NotesDef (NoteLeft Note Nothing Nothing ["","a note","can also be defined","on several lines"])])) 
        
    it "Some other notes" $ do
      let input :: T.Text
          input = [text|
@startuml
participant Alice
participant Bob
note left of Alice #aqua
This is displayed
left of Alice.
end note

note right of Alice: This is displayed right of Alice.

note over Alice: This is displayed over Alice.

note over Alice, Bob #FFAAAA: This is displayed\n over Bob and Alice.

note over Bob, Alice
This is yet another
example of
a long note.
end note
@enduml

|]
      parsePlantUML input `shouldBe` (Right (PlantUML [SubjectDef (Subject Participant (Name1 (Nq "Alice")) Nothing Nothing Nothing),SubjectDef (Subject Participant (Name1 (Nq "Bob")) Nothing Nothing Nothing),NotesDef (NoteLeft Note (Just (Nq "Alice")) (Just (Color Aqua)) ["This is displayed","left of Alice."]),NotesDef (NoteRight Note (Just (Nq "Alice")) Nothing [": This is displayed right of Alice.","note over Alice: This is displayed over Alice.","note over Alice, Bob #FFAAAA: This is displayed\\n over Bob and Alice.","note over Bob, Alice","This is yet another","example of","a long note."])]))

    it "Changing note shape" $ do
      let input :: T.Text
          input = [text|
@startuml
caller -> server : conReq
hnote over caller : idle
caller <- server : conConf
rnote over server
 "r" as rectangle
 "h" as hexagon
endrnote
@enduml
|]
      parsePlantUML input `shouldBe` (Right (PlantUML [ArrowDef (Arrow (Just (Nq "caller")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "server"))) Nothing(Just " conReq")),NotesDef (NoteOver HNote (Nq "caller") Nothing Nothing [" idle"]),ArrowDef (Arrow (Just (Nq "caller")) (Arr (Just "<") (Shaft (Just "-") Nothing Nothing) Nothing) (Just (Name1 (Nq "server"))) Nothing (Just " conConf")),NotesDef (NoteOver RNote (Nq "server") Nothing Nothing ["\"h\" as hexagon"])]))

    it "Creole and HTML" $ do
      let input :: T.Text
          input = [text|
@startuml
participant Alice
participant "The **Famous** Bob" as Bob

Alice -> Bob : hello --there--
... Some ~~long delay~~ ...
Bob -> Alice : ok
note left
  This is **bold**
  This is //italics//
  This is ""monospaced""
  This is --stroked--
  This is __underlined__
  This is ~~waved~~
end note

Alice -> Bob : A //well formatted// message
note right of Alice
 This is <back:cadetblue><size:18>displayed</size></back>
 __left of__ Alice.
end note
note left of Bob
 <u:red>This</u> is <color #118888>displayed</color>
 **<color purple>left of</color> <s:red>Alice</strike> Bob**.
end note
note over Alice, Bob
 <w:#FF33FF>This is hosted</w> by <img sourceforge.jpg>
end note
@enduml
|]
      parsePlantUML input `shouldBe` (Right (PlantUML [SubjectDef (Subject Participant (Name1 (Nq "Alice")) Nothing Nothing Nothing),SubjectDef (Subject Participant (AliasedName (Q "The **Famous** Bob") (Nq "Bob")) Nothing Nothing Nothing),ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing (Just " hello --there--")),CommandDef (Delay (Just "Some ~~long delay~~ ")),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " ok")),NotesDef (NoteLeft Note Nothing Nothing ["","This is **bold**","This is //italics//","This is \"\"monospaced\"\"","This is --stroked--","This is __underlined__","This is ~~waved~~"]),ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing (Just " A //well formatted// message")),NotesDef (NoteRight Note (Just (Nq "Alice")) Nothing ["This is <back:cadetblue><size:18>displayed</size></back>","__left of__ Alice."]),NotesDef (NoteLeft Note (Just (Nq "Bob")) Nothing ["<u:red>This</u> is <color #118888>displayed</color>","**<color purple>left of</color> <s:red>Alice</strike> Bob**."]),NotesDef (NoteOver Note (Nq "Alice") (Just (Nq "Bob")) Nothing [])]))

    it "Divider or separator" $ do
      let input :: T.Text
          input = [text|
@startuml

== Initialization ==

Alice -> Bob: Authentication Request
Bob --> Alice: Authentication Response

== Repetition ==

Alice -> Bob: Another authentication Request
Alice <-- Bob: another authentication Response
@enduml                       
|]
      parsePlantUML input `shouldBe` (Right (PlantUML [CommandDef (Divider " Initialization "),ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing (Just " Authentication Request")),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "--") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " Authentication Response")),CommandDef (Divider " Repetition "),ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing (Just " Another authentication Request")),ArrowDef (Arrow (Just (Nq "Alice")) (Arr (Just "<") (Shaft (Just "--") Nothing Nothing) Nothing) (Just (Name1 (Nq "Bob"))) Nothing (Just " another authentication Response"))]))

    it "Reference" $ do
      let input :: T.Text
          input = [text|
@startuml
participant Alice
actor Bob

ref over Alice, Bob : init

Alice -> Bob : hello

ref over Bob
  This can be on
  several lines
end ref
@enduml
|]
      parsePlantUML input `shouldBe` (Right (PlantUML [SubjectDef (Subject Participant (Name1 (Nq "Alice")) Nothing Nothing Nothing),SubjectDef (Subject Actor (Name1 (Nq "Bob")) Nothing Nothing Nothing),NotesDef (RefOver (Nq "Alice") (Just (Nq "Bob")) Nothing [" init"]),ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing (Just " hello")),NotesDef (RefOver (Nq "Bob") Nothing Nothing ["several lines"])]))


    it "Delay" $ do
      let input :: T.Text
          input = [text|
@startuml

Alice -> Bob: Authentication Request
...
Bob --> Alice: Authentication Response
...5 minutes later...
Bob --> Alice: Good Bye !

@enduml
|]
      parsePlantUML input `shouldBe` (Right (PlantUML [ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing (Just " Authentication Request")),CommandDef (Delay Nothing),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "--") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " Authentication Response")),CommandDef (Delay (Just "5 minutes later")),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "--") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " Good Bye !"))])) 

    it "Text wrapping" $ do
      let input :: T.Text
          input = [text|
@startuml
skinparam maxMessageSize 50
participant a
participant b
a -> b :this\nis\nmanually\ndone
a -> b :this is a very long message on several words
@enduml
|]
      parsePlantUML input `shouldBe`
        (Right (PlantUML [
                   CommandDef (SkinParameters [MaxMessageSize 50]),
                   SubjectDef (Subject Participant (Name1 (Nq "a")) Nothing Nothing Nothing),
                   SubjectDef (Subject Participant (Name1 (Nq "b")) Nothing Nothing Nothing),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "b"))) Nothing (Just "this\\nis\\nmanually\\ndone")),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">"))  (Just (Name1 (Nq "b"))) Nothing (Just "this is a very long message on several words"))]))

    it "Space" $ do
      let input :: T.Text
          input = [text|
@startuml

Alice -> Bob: message 1
Bob --> Alice: ok
|||
Alice -> Bob: message 2
Bob --> Alice: ok
||45||
Alice -> Bob: message 3
Bob --> Alice: ok

@enduml
|]
      parsePlantUML input `shouldBe` (Right (PlantUML [ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing (Just " message 1")),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "--") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " ok")),CommandDef (Spacer Nothing),ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing (Just " message 2")),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "--") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " ok")),CommandDef (Spacer (Just 45)),ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing (Just " message 3")),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "--") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " ok"))]))

    it "Lifeline activation and destruction 1" $ do
      let input :: T.Text
          input = [text|
@startuml
participant User

User -> A: DoWork
activate A

A -> B: << createRequest >>
activate B

B -> C: DoWork
activate C
C --> B: WorkDone
destroy C

B --> A: RequestCreated
deactivate B

A -> User: Done
deactivate A

@enduml
|]
      parsePlantUML input `shouldBe`
        (Right (PlantUML [
                   SubjectDef (Subject Participant (Name1 (Nq "User")) Nothing Nothing Nothing),ArrowDef (Arrow (Just (Nq "User")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "A"))) Nothing (Just " DoWork")),CommandDef (Activate (Nq "A") Nothing),ArrowDef (Arrow (Just (Nq "A")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just(Name1 (Nq "B"))) Nothing (Just " << createRequest >>")),CommandDef (Activate (Nq "B") Nothing),ArrowDef (Arrow (Just (Nq "B")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "C"))) Nothing (Just " DoWork")),CommandDef (Activate (Nq "C") Nothing),ArrowDef (Arrow (Just (Nq "C")) (Arr Nothing (Shaft (Just "--") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "B"))) Nothing (Just " WorkDone")),CommandDef (LifeLine Destroy Nothing (Nq "C")),ArrowDef (Arrow (Just (Nq "B")) (Arr Nothing (Shaft (Just "--") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "A"))) Nothing (Just " RequestCreated")),CommandDef (Deactivate (Nq "B")),ArrowDef (Arrow (Just (Nq "A")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "User"))) Nothing (Just " Done")),CommandDef (Deactivate (Nq "A"))]))
    it "Lifeline activation and destruction 2" $ do
      let input :: T.Text
          input = [text|
@startuml
participant User

User -> A: DoWork
activate A #FFBBBB

A -> A: Internal call
activate A #DarkSalmon

A -> B: << createRequest >>
activate B

B --> A: RequestCreated
deactivate B
deactivate A
A -> User: Done
deactivate A

@enduml
|]
      parsePlantUML input `shouldBe` (Right (PlantUML [SubjectDef (Subject Participant (Name1 (Nq "User")) Nothing Nothing Nothing),ArrowDef (Arrow (Just (Nq "User")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "A"))) Nothing (Just " DoWork")),CommandDef (Activate (Nq "A") (Just (HexColor "FFBBBB"))),ArrowDef (Arrow (Just (Nq "A")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "A"))) Nothing (Just " Internal call")),CommandDef (Activate (Nq "A") (Just (Color DarkSalmon))),ArrowDef (Arrow (Just (Nq "A")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "B"))) Nothing (Just " << createRequest >>")),CommandDef (Activate (Nq "B") Nothing),ArrowDef (Arrow (Just (Nq "B")) (Arr Nothing (Shaft (Just "--") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "A"))) Nothing (Just " RequestCreated")),CommandDef (Deactivate (Nq "B")),CommandDef (Deactivate (Nq "A")),ArrowDef (Arrow (Just (Nq "A")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "User"))) Nothing (Just " Done")),CommandDef (Deactivate (Nq "A"))])      )
    it "Lifeline activation and destruction 3" $ do
      let input :: T.Text
          input = [text|
@startuml
autoactivate on
alice -> bob : hello
bob -> bob : self call
bill -> bob #005500 : hello from thread 2
bob -> george ** : create
return done in thread 2
return rc
bob -> george !! : delete
return success

@enduml
|]
      parsePlantUML input `shouldBe` (Right (PlantUML [CommandDef (AutoActivate On),ArrowDef (Arrow (Just (Nq "alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "bob"))) Nothing (Just " hello")),ArrowDef (Arrow (Just (Nq "bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "bob"))) Nothing (Just " self call")),ArrowDef (Arrow (Just (Nq "bill")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "bob"))) (Just (HexColor "005500")) (Just " hello from thread 2")),ArrowDef (ActivationArrow (Just (Nq "bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Nq "george") Creation (Just " create")),ArrowDef (Return (Just "done in thread 2")),ArrowDef (Return (Just "rc")),ArrowDef (ActivationArrow (Just (Nq "bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Nq "george") Destruction (Just " delete")),ArrowDef (Return (Just "success"))]))

    it "Return" $ do
      let input :: T.Text
          input = [text|
@startuml
Bob -> Alice : hello
activate Alice
Alice -> Alice : some action
return bye
@enduml
|]
      parsePlantUML input `shouldBe` (Right (PlantUML [ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " hello")),CommandDef (Activate (Nq "Alice") Nothing),ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " some action")),ArrowDef (Return (Just "bye"))])) 
    it "Participant creation" $ do
      let input :: T.Text
          input = [text|
@startuml
Bob -> Alice : hello

create Other
Alice -> Other : new

create control String
Alice -> String
note right : You can also put notes!

Alice --> Bob : ok

@enduml
|]
      parsePlantUML input `shouldBe` (Right (PlantUML [ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " hello")),CommandDef (LifeLine Create Nothing (Nq "Other")),ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Other"))) Nothing (Just " new")),CommandDef (LifeLine Create (Just Control) (Nq "String")),ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "String"))) Nothing Nothing),NotesDef (NoteRight Note Nothing Nothing [" You can also put notes!"]),ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "--") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing (Just " ok"))]))
    it "Shortcut syntax for activation, deactivation and creation" $ do
      let input :: T.Text
          input = [text|
@startuml
alice -> bob ++ : hello
bob -> bob ++ : self call
bob -> bib ++  #005500 : hello
bob -> george ** : create
return done
return rc
bob -> george !! : delete
return success
@enduml
|]
      parsePlantUML input `shouldBe` (Right (PlantUML [ArrowDef (ActivationArrow (Just (Nq "alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Nq "bob") (Activation Nothing) (Just " hello")),ArrowDef (ActivationArrow (Just (Nq "bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Nq "bob") (Activation Nothing) (Just " self call")),ArrowDef (ActivationArrow (Just (Nq "bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Nq "bib") (Activation (Just (HexColor "005500"))) (Just " hello")),ArrowDef (ActivationArrow (Just (Nq "bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Nq "george") Creation (Just " create")),ArrowDef (Return (Just "done")),ArrowDef (Return (Just "rc")),ArrowDef (ActivationArrow (Just (Nq "bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Nq "george") Destruction (Just " delete")),ArrowDef (Return (Just "success"))]))
      
    it "Incoming and outgoing mesages 1" $ do
      let input :: T.Text
          input = [text|
@startuml
[-> A: DoWork

activate A

A -> A: Internal call
activate A

A ->] : << createRequest >>

A<--] : RequestCreated
deactivate A
[<- A: Done
deactivate A
@enduml
|]
      parsePlantUML input `shouldBe` ( Right (PlantUML [ArrowDef (Arrow Nothing (Arr (Just "[") (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "A"))) Nothing (Just " DoWork")),CommandDef (Activate (Nq "A") Nothing),ArrowDef (Arrow (Just (Nq "A")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "A"))) Nothing (Just " Internal call")),CommandDef (Activate (Nq "A") Nothing),ArrowDef (Arrow (Just (Nq "A")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">]")) Nothing Nothing (Just " << createRequest >>")),ArrowDef (Arrow (Just (Nq "A")) (Arr (Just "<") (Shaft (Just "--") Nothing Nothing) (Just "]")) Nothing Nothing (Just " RequestCreated")),CommandDef (Deactivate (Nq "A")),ArrowDef (Arrow Nothing (Arr (Just "[<") (Shaft (Just "-") Nothing Nothing) Nothing) (Just (Name1 (Nq "A"))) Nothing (Just " Done")),CommandDef (Deactivate (Nq "A"))]))
    it "Incoming and outgoing messages 2" $ do
      let input :: T.Text
          input = [text|
@startuml
participant Alice
participant Bob #lightblue
Alice -> Bob
Bob -> Carol
...
Bob ->o]
[-> Bob
[o-> Bob
[o->o Bob
[x-> Bob
...
[<- Bob
[x<- Bob
...
Bob ->]
Bob ->o]
Bob o->o]
Bob ->x]
...
Bob <-]
Bob x<-]

@enduml
|]
      parsePlantUML input `shouldBe` (Right (PlantUML [SubjectDef (Subject Participant (Name1 (Nq "Alice")) Nothing Nothing Nothing),SubjectDef (Subject Participant (Name1 (Nq "Bob")) Nothing Nothing (Just (Color LightBlue))),ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing Nothing),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Carol"))) Nothing Nothing),CommandDef (Delay Nothing),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">o]")) Nothing Nothing Nothing),ArrowDef (Arrow Nothing (Arr (Just "[") (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing Nothing),ArrowDef (Arrow Nothing (Arr (Just "[o") (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing Nothing),ArrowDef (Arrow Nothing (Arr (Just "[o") (Shaft (Just "-") Nothing Nothing) (Just ">o")) (Just (Name1 (Nq "Bob"))) Nothing Nothing),ArrowDef (Arrow Nothing (Arr (Just "[x") (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing Nothing),CommandDef (Delay Nothing),ArrowDef (Arrow Nothing (Arr (Just "[<") (Shaft (Just "-") Nothing Nothing) Nothing) (Just (Name1 (Nq "Bob"))) Nothing Nothing),ArrowDef (Arrow Nothing (Arr (Just "[x<") (Shaft (Just "-") Nothing Nothing) Nothing) (Just (Name1 (Nq "Bob"))) Nothing Nothing),CommandDef (Delay Nothing),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">]")) (Just (Name1 (Nq "Bob"))) Nothing Nothing),ArrowDef (Arrow Nothing (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">o]")) (Just (Name1 (Nq "Bob"))) Nothing Nothing),ArrowDef (Arrow (Just (Nq "o")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">o]")) (Just (Name1 (Nq "Bob"))) Nothing Nothing),ArrowDef (Arrow Nothing (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">x]")) Nothing Nothing Nothing),CommandDef (Delay Nothing),ArrowDef (Arrow (Just (Nq "Bob")) (Arr (Just "<") (Shaft (Just "-") Nothing Nothing) (Just "]")) (Just (Name1 (Nq "Bob"))) Nothing Nothing),ArrowDef (Arrow (Just (Nq "x")) (Arr (Just "<") (Shaft (Just "-") Nothing Nothing) (Just "]")) Nothing Nothing Nothing)]))
    it "Short arrows for incomming and outgoing messages" $ do
      let input :: T.Text
          input = [text|
@startuml
?-> Alice    : ""?->""\n**short** to actor1
[-> Alice    : ""[->""\n**from start** to actor1
[-> Bob      : ""[->""\n**from start** to actor2
?-> Bob      : ""?->""\n**short** to actor2
Alice ->]    : ""->]""\nfrom actor1 **to end**
Alice ->?    : ""->?""\n**short** from actor1
Alice -> Bob : ""->"" \nfrom actor1 to actor2
@enduml
|]
      parsePlantUML input `shouldBe` (Right (PlantUML [ArrowDef (Arrow Nothing (Arr (Just "?") (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " \"\"?->\"\"\\n**short** to actor1")),ArrowDef (Arrow Nothing (Arr (Just "[") (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " \"\"[->\"\"\\n**from start** to actor1")),ArrowDef (Arrow Nothing (Arr (Just "[") (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing (Just " \"\"[->\"\"\\n**from start** to actor2")),ArrowDef (Arrow Nothing (Arr (Just "?") (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing (Just " \"\"?->\"\"\\n**short** to actor2")),ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">]")) Nothing Nothing (Just " \"\"->]\"\"\\nfrom actor1 **to end**")),ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">?")) Nothing Nothing (Just " \"\"->?\"\"\\n**short** from actor1")),ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing (Just " \"\"->\"\" \\nfrom actor1 to actor2"))]))

    it "Anchors and duration" $ do
      let input :: T.Text
          input = [text|
@startuml
!pragma teoz true

{start} Alice -> Bob : start doing things during duration
Bob -> Max : something
Max -> Bob : something else
{end} Bob -> Alice : finish

{start} <-> {end} : some time

@enduml
|]
      parsePlantUML input `shouldBe` (Right (PlantUML []))

    it "Stereotype and spots 1" $ do
      let input :: T.Text
          input = [text|
@startuml

participant "Famous Bob" as Bob << Generated >>
participant Alice << (C,#ADD1B2) Testable >>

Bob->Alice: First message

@enduml
|]
      parsePlantUML input `shouldBe` (Right (PlantUML [SubjectDef (Subject Participant (AliasedName (Q "Famous Bob") (Nq "Bob")) (Just (Stereotype " Generated ")) Nothing Nothing),SubjectDef (Subject Participant (Name1 (Nq "Alice")) (Just (Stereotype " (C,#ADD1B2) Testable ")) Nothing Nothing),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " First message"))]))

    it "Stereotype and spots 2" $ do
      let input :: T.Text
          input = [text|
@startuml

skinparam guillemet false
participant "Famous Bob" as Bob << Generated >>
participant Alice << (C,#ADD1B2) Testable >>

Bob->Alice: First message

@enduml
|]
      parsePlantUML input `shouldBe` (Right (PlantUML [CommandDef (SkinParameters [Guillemet False]),SubjectDef (Subject Participant (AliasedName (Q "Famous Bob") (Nq "Bob")) (Just (Stereotype " Generated ")) Nothing Nothing),SubjectDef (Subject  Participant (Name1 (Nq "Alice")) (Just (Stereotype " (C,#ADD1B2) Testable ")) Nothing Nothing),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " First message"))]))

    it "Stereotype and spots 3" $ do
      let input :: T.Text
          input = [text|
@startuml

participant Bob << (C,#ADD1B2) >>
participant Alice << (C,#ADD1B2) >>

Bob->Alice: First message

@enduml
|]
      parsePlantUML input `shouldBe` (Right (PlantUML [SubjectDef (Subject Participant (Name1 (Nq "Bob")) (Just (Stereotype " (C,#ADD1B2) ")) Nothing Nothing),SubjectDef (Subject Participant (Name1 (Nq "Alice")) (Just (Stereotype " (C,#ADD1B2) ")) Nothing Nothing),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " First message"))]))

    it "More information on titles 1" $ do
      let input :: T.Text
          input = [text|
@startuml

title __Simple__ **communication** example

Alice -> Bob: Authentication Request
Bob -> Alice: Authentication Response

@enduml
|]
      parsePlantUML input `shouldBe` (Right (PlantUML [CommandDef (Title " __Simple__ **communication** example"),ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing (Just " Authentication Request")),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " Authentication Response"))]))


    it "More information on titles 2" $ do
      let input :: T.Text
          input = [text|
@startuml

title __Simple__ communication example\non several lines

Alice -> Bob: Authentication Request
Bob -> Alice: Authentication Response

@enduml
|]
      parsePlantUML input `shouldBe` (Right (PlantUML [CommandDef (Title " __Simple__ communication example\\non several lines"),ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing (Just " Authentication Request")),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " Authentication Response"))]))

    it "More information on titles 3" $ do
      let input :: T.Text
          input = [text|
@startuml

title
 <u>Simple</u> communication example
 on <i>several</i> lines and using <font color=red>html</font>
 This is hosted by <img:sourceforge.jpg>
end title

Alice -> Bob: Authentication Request
Bob -> Alice: Authentication Response

@enduml
|]
      parsePlantUML input `shouldBe` (Right (PlantUML [CommandDef (Title "<u>Simple</u> communication exampleon <i>several</i> lines and using <font color=red>html</font>This is hosted by <img:sourceforge.jpg>"),ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing (Just " Authentication Request")),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " Authentication Response"))]))

    it "Participants encompass" $ do
      let input :: T.Text
          input = [text|
@startuml

box "Internal Service" #LightBlue
participant Bob
participant Alice
end box
participant Other

Bob -> Alice : hello
Alice -> Other : hello

@enduml
|]
      parsePlantUML input `shouldBe` ( Right (PlantUML [BoxDef (Box (Just (Q "Internal Service")) (Just (Color LightBlue)) [SubjectDef (Subject Participant (Name1 (Nq "Bob")) Nothing Nothing Nothing),SubjectDef (Subject Participant (Name1 (Nq "Alice")) Nothing Nothing Nothing)]),SubjectDef (Subject Participant (Name1 (Nq "Other")) Nothing Nothing Nothing),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " hello")),ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Other"))) Nothing (Just " hello"))]))

    it "Removing footboxes" $ do
      let input :: T.Text
          input = [text|
@startuml

hide footbox
title Foot Box removed

Alice -> Bob: Authentication Request
Bob --> Alice: Authentication Response

@enduml
|]
      parsePlantUML input `shouldBe` (Right (PlantUML [CommandDef (Hide FootBox),CommandDef (Title " Foot Box removed"),ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing (Just " Authentication Request")),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "--") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " Authentication Response"))])) 

    it "Skinparam 1" $ do
      let input :: T.Text
          input = [text|
@startuml
skinparam sequenceArrowThickness 2
skinparam roundcorner 20
skinparam maxmessagesize 60
skinparam sequenceParticipant underline

actor User
participant "First Class" as A
participant "Second Class" as B
participant "Last Class" as C

User -> A: DoWork
activate A

A -> B: Create Request
activate B

B -> C: DoWork
activate C
C --> B: WorkDone
destroy C

B --> A: Request Created
deactivate B

A --> User: Done
deactivate A

@enduml
|]
      parsePlantUML input `shouldBe`
        (Right (PlantUML [
                   CommandDef (SkinParameters [SequenceArrowThickness 2]),
                   CommandDef (SkinParameters [RoundCorner 20]),
                   CommandDef (SkinParameters [MaxMessageSize 60]),
                   CommandDef (SkinParameters [SequenceParticipant Underline]),
                   SubjectDef (Subject Actor (Name1 (Nq "User")) Nothing Nothing Nothing),SubjectDef (Subject Participant (AliasedName (Q "First Class") (Nq "A")) Nothing Nothing Nothing),SubjectDef (Subject Participant (AliasedName (Q "Second Class") (Nq "B")) Nothing Nothing Nothing),SubjectDef (Subject Participant (AliasedName (Q "Last Class") (Nq "C")) Nothing Nothing Nothing),ArrowDef (Arrow (Just (Nq "User")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "A"))) Nothing (Just " DoWork")),CommandDef (Activate (Nq "A") Nothing),ArrowDef (Arrow (Just (Nq "A")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "B"))) Nothing (Just " Create Request")),CommandDef (Activate (Nq "B") Nothing),ArrowDef (Arrow (Just (Nq "B")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "C"))) Nothing (Just " DoWork")),CommandDef (Activate (Nq "C") Nothing),ArrowDef (Arrow (Just (Nq "C")) (Arr Nothing (Shaft (Just "--") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "B"))) Nothing (Just " WorkDone")),CommandDef (LifeLine Destroy Nothing (Nq "C")),ArrowDef (Arrow (Just (Nq "B")) (Arr Nothing (Shaft (Just "--") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "A"))) Nothing (Just " Request Created")),CommandDef (Deactivate (Nq "B")),ArrowDef (Arrow (Just (Nq "A")) (Arr Nothing (Shaft (Just "--") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "User"))) Nothing (Just " Done")),CommandDef (Deactivate (Nq "A"))]))

    it "Skinparams 2" $ do
      let input :: T.Text
          input = [text|
@startuml
skinparam backgroundColor #EEEBDC
skinparam handwritten true

skinparam sequence {
ArrowColor DeepSkyBlue
ActorBorderColor DeepSkyBlue
LifeLineBorderColor blue
LifeLineBackgroundColor #A9DCDF

ParticipantBorderColor DeepSkyBlue
ParticipantBackgroundColor DodgerBlue
ParticipantFontName Impact
ParticipantFontSize 17
ParticipantFontColor #A9DCDF

ActorBackgroundColor aqua
ActorFontColor DeepSkyBlue
ActorFontSize 17
ActorFontName Aapex
}

actor User
participant "First Class" as A
participant "Second Class" as B
participant "Last Class" as C

User -> A: DoWork
activate A

A -> B: Create Request
activate B

B -> C: DoWork
activate C
C --> B: WorkDone
destroy C

B --> A: Request Created
deactivate B

A --> User: Done
deactivate A

@enduml

|]
            
      parsePlantUML input `shouldBe` (Right (PlantUML [CommandDef (SkinParameters [BackgroundColor (HexColor "EEEBDC")]),CommandDef (SkinParameters [Handwritten True]),CommandDef (SkinParameters [ArrowColor (Color DeepSkyBlue),ActorBorderColor (Color DeepSkyBlue),LifeLineBorderColor (Color Blue),LifeLineBackgroundColor (HexColor "A9DCDF"),ParticipantBorderColor (Color DeepSkyBlue),ParticipantBackgroundColor (Color DodgerBlue),ParticipantFontName "Impact",ParticipantFontSize 17,ParticipantFontColor (HexColor "A9DCDF"),ActorBackgroundColor (Color Aqua),ActorFontColor (Color DeepSkyBlue),ActorFontSize 17,ActorFontName "Aapex"]),SubjectDef (Subject Actor (Name1 (Nq "User")) Nothing Nothing Nothing),SubjectDef (Subject Participant (AliasedName (Q "First Class") (Nq "A")) Nothing Nothing Nothing),SubjectDef (Subject Participant (AliasedName (Q "Second Class") (Nq "B")) Nothing Nothing Nothing),SubjectDef (Subject Participant (AliasedName (Q "Last Class") (Nq "C")) Nothing Nothing Nothing),ArrowDef (Arrow (Just (Nq "User")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "A"))) Nothing (Just " DoWork")),CommandDef (Activate (Nq "A") Nothing),ArrowDef (Arrow (Just (Nq "A")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "B"))) Nothing (Just " Create Request")),CommandDef (Activate (Nq "B") Nothing),ArrowDef (Arrow (Just (Nq "B")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "C"))) Nothing (Just " DoWork")),CommandDef (Activate (Nq "C") Nothing),ArrowDef (Arrow (Just (Nq "C")) (Arr Nothing (Shaft (Just "--") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "B"))) Nothing (Just " WorkDone")),CommandDef (LifeLine Destroy Nothing (Nq "C")),ArrowDef (Arrow (Just (Nq "B")) (Arr Nothing (Shaft (Just "--") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "A"))) Nothing (Just " Request Created")),CommandDef (Deactivate (Nq "B")),ArrowDef (Arrow (Just (Nq "A")) (Arr Nothing (Shaft (Just "--") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "User"))) Nothing (Just " Done")),CommandDef (Deactivate (Nq "A"))]))

    it "Changing padding" $ do
      let input :: T.Text
          input = [text|
@startuml
skinparam ParticipantPadding 20
skinparam BoxPadding 10

box "Foo1"
participant Alice1
participant Alice2
end box
box "Foo2"
participant Bob1
participant Bob2
end box
Alice1 -> Bob1 : hello
Alice1 -> Out : out
@enduml
|]
      parsePlantUML input `shouldBe` (Right (PlantUML [CommandDef (SkinParameters [ParticipantPadding 20]),CommandDef (SkinParameters [BoxPadding 10]),BoxDef (Box (Just (Q "Foo1")) Nothing [SubjectDef (Subject Participant (Name1 (Nq "Alice1")) Nothing Nothing Nothing),SubjectDef (Subject Participant (Name1 (Nq "Alice2")) Nothing Nothing Nothing)]),BoxDef (Box (Just (Q "Foo2")) Nothing [SubjectDef (Subject Participant (Name1 (Nq "Bob1")) Nothing Nothing Nothing),SubjectDef (Subject Participant (Name1 (Nq "Bob2")) Nothing Nothing Nothing)]),ArrowDef (Arrow (Just (Nq "Alice1")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob1"))) Nothing (Just " hello")),ArrowDef (Arrow (Just (Nq "Alice1")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Out"))) Nothing (Just " out"))]))
    it "Trivial" $ do
      let input :: T.Text
          input = [text|
@startuml
@enduml
|]

      parsePlantUML input `shouldBe` (Right (PlantUML []))
    it "Appendix Examples of all allow type" $ do
      let input :: T.Text
          input = [text|
@startuml
participant Alice as a
participant Bob   as b
a ->     b : ""->   ""
a ->>    b : ""->>  ""
a -\     b : ""-\   ""
a -\\    b : ""-\\\\""
a -/     b : ""-/   ""
a -//    b : ""-//  ""
a ->x    b : ""->x  ""
a x->    b : ""x->  ""
a o->    b : ""o->  ""
a ->o    b : ""->o  ""
a o->o   b : ""o->o ""
a <->    b : ""<->  ""
a o<->o  b : ""o<->o""
a x<->x  b : ""x<->x""
a ->>o   b : ""->>o ""
a -\o    b : ""-\o  ""
a -\\o   b : ""-\\\\o""
a -/o    b : ""-/o  ""
a -//o   b : ""-//o ""
a x->o   b : ""x->o ""
@enduml
|]
      parsePlantUML input `shouldBe` (Right (PlantUML [SubjectDef (Subject Participant (AliasedName (Nq "Alice") (Nq "a")) Nothing Nothing Nothing),SubjectDef (Subject Participant (AliasedName (Nq "Bob") (Nq "b")) Nothing Nothing Nothing),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "b"))) Nothing (Just " \"\"->   \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">>")) (Just (Name1 (Nq "b"))) Nothing (Just " \"\"->>  \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just "\\")) (Just (Name1 (Nq "b"))) Nothing (Just " \"\"-\\   \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just "\\")) (Just (Name1 (Nq "b"))) Nothing (Just " \"\"-\\\\\"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just "/")) (Just (Name1 (Nq "b"))) Nothing (Just " \"\"-/   \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just "//")) (Just (Name1 (Nq "b"))) Nothing (Just " \"\"-//  \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">x")) (Just (Name1 (Nq "b"))) Nothing (Just " \"\"->x  \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr (Just "x") (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "b"))) Nothing (Just " \"\"x->  \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr (Just "o") (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "b"))) Nothing (Just " \"\"o->  \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">o")) (Just (Name1 (Nq "b"))) Nothing (Just " \"\"->o  \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr (Just "o") (Shaft (Just "-") Nothing Nothing) (Just ">o")) (Just (Name1 (Nq "b"))) Nothing (Just " \"\"o->o \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr (Just "<") (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "b"))) Nothing (Just " \"\"<->  \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr (Just "o<") (Shaft (Just "-") Nothing Nothing) (Just ">o")) (Just (Name1 (Nq "b"))) Nothing (Just " \"\"o<->o\"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr (Just "x<") (Shaft (Just "-") Nothing Nothing) (Just ">x")) (Just (Name1 (Nq "b"))) Nothing (Just " \"\"x<->x\"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">>o")) (Just (Name1 (Nq "b"))) Nothing (Just " \"\"->>o \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just "\\o")) (Just (Name1 (Nq "b"))) Nothing (Just " \"\"-\\o  \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just "\\o")) (Just (Name1 (Nq "b"))) Nothing (Just " \"\"-\\\\o\"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just "/o")) (Just (Name1 (Nq "b"))) Nothing (Just " \"\"-/o  \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just "//o")) (Just (Name1 (Nq "b"))) Nothing (Just " \"\"-//o \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr (Just "x") (Shaft (Just "-") Nothing Nothing) (Just ">o")) (Just (Name1 (Nq "b"))) Nothing (Just " \"\"x->o \"\""))]))

    it "Incomming and outgoing messages (with [,]). Incomming messages" $ do
      let input :: T.Text
          input = [text|
@startuml
participant Alice as a
participant Bob   as b
a ->]      : ""->]   ""
a ->>]     : ""->>]  ""
a -\]      : ""-\]   ""
a -\\]     : ""-\\\\]""
a -/]      : ""-/]   ""
a -//]     : ""-//]  ""
a ->x]     : ""->x]  ""
a x->]     : ""x->]  ""
a o->]     : ""o->]  ""
a ->o]     : ""->o]  ""
a o->o]    : ""o->o] ""
a <->]     : ""<->]  ""
a o<->o]   : ""o<->o]""
a x<->x]   : ""x<->x]""
a ->>o]    : ""->>o] ""
a -\o]     : ""-\o]  ""
a -\\o]    : ""-\\\\o]""
a -/o]     : ""-/o]  ""
a -//o]    : ""-//o] ""
a x->o]    : ""x->o] ""
@enduml                       
|]
      parsePlantUML input `shouldBe` (Right (PlantUML [SubjectDef (Subject Participant (AliasedName (Nq "Alice") (Nq "a")) Nothing Nothing Nothing),SubjectDef (Subject Participant (AliasedName (Nq "Bob") (Nq "b")) Nothing Nothing Nothing),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">]")) Nothing Nothing (Just " \"\"->]   \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">>]")) Nothing Nothing (Just " \"\"->>]  \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just "\\]")) Nothing Nothing (Just " \"\"-\\]   \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just "\\]")) Nothing Nothing (Just " \"\"-\\\\]\"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just "/]")) Nothing Nothing (Just " \"\"-/]   \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just "//]")) Nothing Nothing (Just " \"\"-//]  \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">x]")) Nothing Nothing (Just " \"\"->x]  \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr (Just "x") (Shaft (Just "-") Nothing Nothing) (Just ">]")) Nothing Nothing (Just " \"\"x->]  \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr (Just "o") (Shaft (Just "-") Nothing Nothing) (Just ">]")) Nothing Nothing (Just " \"\"o->]  \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">o]")) Nothing Nothing (Just " \"\"->o]  \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr (Just "o") (Shaft (Just "-") Nothing Nothing) (Just ">o]")) Nothing Nothing (Just " \"\"o->o] \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr (Just "<") (Shaft (Just "-") Nothing Nothing) (Just ">]")) Nothing Nothing (Just " \"\"<->]  \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr (Just "o<") (Shaft (Just "-") Nothing Nothing) (Just ">o]")) Nothing Nothing (Just " \"\"o<->o]\"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr (Just "x<") (Shaft (Just "-") Nothing Nothing) (Just ">x]")) Nothing Nothing (Just " \"\"x<->x]\"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">>o]")) Nothing Nothing (Just " \"\"->>o] \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just "\\o]")) Nothing Nothing (Just " \"\"-\\o]  \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just "\\o]")) Nothing Nothing (Just " \"\"-\\\\o]\"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just "/o]")) Nothing Nothing (Just " \"\"-/o]  \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just "//o]")) Nothing Nothing (Just " \"\"-//o] \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr (Just "x") (Shaft (Just "-") Nothing Nothing) (Just ">o]")) Nothing Nothing (Just " \"\"x->o] \"\""))]))

    it "Incomming and outgoing messages (with [,]). Outgoing messages" $ do
      let input :: T.Text
          input = [text|
@startuml
participant Alice as a
participant Bob   as b
a ->     b : //Long long label//
?->      b : ""?->   ""
?->>     b : ""?->>  ""
?-\      b : ""?-\   ""
?-\\     b : ""?-\\\\""
?-/      b : ""?-/   ""
?-//     b : ""?-//  ""
?->x     b : ""?->x  ""
?x->     b : ""?x->  ""
?o->     b : ""?o->  ""
?->o     b : ""?->o  ""
?o->o    b : ""?o->o ""
?<->     b : ""?<->  ""
?o<->o   b : ""?o<->o""
?x<->x   b : ""?x<->x""
?->>o    b : ""?->>o ""
?-\o     b : ""?-\o  ""
?-\\o    b : ""?-\\\\o ""
?-/o     b : ""?-/o  ""
?-//o    b : ""?-//o ""
?x->o    b : ""?x->o ""
@enduml
|]
      parsePlantUML input `shouldBe` (Right (PlantUML [SubjectDef (Subject Participant (AliasedName (Nq "Alice") (Nq "a")) Nothing Nothing Nothing),SubjectDef (Subject Participant (AliasedName (Nq "Bob") (Nq "b")) Nothing Nothing Nothing),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "b"))) Nothing (Just " //Long long label//")),ArrowDef (Arrow Nothing (Arr (Just "?") (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "b"))) Nothing (Just " \"\"?->   \"\"")),ArrowDef (Arrow Nothing (Arr (Just "?") (Shaft (Just "-") Nothing Nothing) (Just ">>")) (Just (Name1 (Nq "b"))) Nothing (Just " \"\"?->>  \"\"")),ArrowDef (Arrow Nothing (Arr (Just "?") (Shaft (Just "-") Nothing Nothing) (Just "\\")) (Just (Name1 (Nq "b"))) Nothing (Just " \"\"?-\\   \"\"")),ArrowDef (Arrow Nothing (Arr (Just "?") (Shaft (Just "-") Nothing Nothing) (Just "\\")) (Just (Name1 (Nq "b"))) Nothing (Just " \"\"?-\\\\\"\"")),ArrowDef (Arrow Nothing (Arr (Just "?") (Shaft (Just "-") Nothing Nothing) (Just "/")) (Just (Name1 (Nq "b"))) Nothing (Just " \"\"?-/   \"\"")),ArrowDef (Arrow Nothing (Arr (Just "?") (Shaft (Just "-") Nothing Nothing) (Just "//")) (Just (Name1 (Nq "b"))) Nothing (Just " \"\"?-//  \"\"")),ArrowDef (Arrow Nothing (Arr (Just "?") (Shaft (Just "-") Nothing Nothing) (Just ">x")) (Just (Name1 (Nq "b"))) Nothing (Just " \"\"?->x  \"\"")),ArrowDef (Arrow Nothing (Arr (Just "?x") (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "b"))) Nothing (Just " \"\"?x->  \"\"")),ArrowDef (Arrow Nothing (Arr (Just "?o") (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "b"))) Nothing (Just " \"\"?o->  \"\"")),ArrowDef (Arrow Nothing (Arr (Just "?") (Shaft (Just "-") Nothing Nothing) (Just ">o")) (Just (Name1 (Nq "b"))) Nothing (Just " \"\"?->o  \"\"")),ArrowDef (Arrow Nothing (Arr (Just "?o") (Shaft (Just "-") Nothing Nothing) (Just ">o")) (Just (Name1 (Nq "b"))) Nothing (Just " \"\"?o->o \"\"")),ArrowDef (Arrow Nothing (Arr (Just "?<") (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "b"))) Nothing (Just " \"\"?<->  \"\"")),ArrowDef (Arrow Nothing (Arr (Just "?o<") (Shaft (Just "-") Nothing Nothing) (Just ">o")) (Just (Name1 (Nq "b"))) Nothing (Just " \"\"?o<->o\"\"")),ArrowDef (Arrow Nothing (Arr (Just "?x<") (Shaft (Just "-") Nothing Nothing) (Just ">x")) (Just (Name1 (Nq "b"))) Nothing (Just " \"\"?x<->x\"\"")),ArrowDef (Arrow Nothing (Arr (Just "?") (Shaft (Just "-") Nothing Nothing) (Just ">>o")) (Just (Name1 (Nq "b"))) Nothing (Just " \"\"?->>o \"\"")),ArrowDef (Arrow Nothing (Arr (Just "?") (Shaft (Just "-") Nothing Nothing) (Just "\\o")) (Just (Name1 (Nq "b"))) Nothing (Just " \"\"?-\\o  \"\"")),ArrowDef (Arrow Nothing (Arr (Just "?") (Shaft (Just "-") Nothing Nothing) (Just "\\o")) (Just (Name1 (Nq "b"))) Nothing (Just " \"\"?-\\\\o \"\"")),ArrowDef (Arrow Nothing (Arr (Just "?") (Shaft (Just "-") Nothing Nothing) (Just "/o")) (Just (Name1 (Nq "b"))) Nothing (Just " \"\"?-/o  \"\"")),ArrowDef (Arrow Nothing (Arr (Just "?") (Shaft (Just "-") Nothing Nothing) (Just "//o")) (Just (Name1 (Nq "b"))) Nothing (Just " \"\"?-//o \"\"")),ArrowDef (Arrow Nothing (Arr (Just "?x") (Shaft (Just "-") Nothing Nothing) (Just ">o")) (Just (Name1 (Nq "b"))) Nothing (Just " \"\"?x->o \"\""))]))

    it "Short outgoing (with '?')" $ do
      let input :: T.Text
          input = [text|
@startuml
participant Alice as a
participant Bob   as b
a ->     b : //Long long label//
a ->?      : ""->?   ""
a ->>?     : ""->>?  ""
a -\?      : ""-\?   ""
a -\\?     : ""-\\\\?""
a -/?      : ""-/?   ""
a -//?     : ""-//?  ""
a ->x?     : ""->x?  ""
a x->?     : ""x->?  ""
a o->?     : ""o->?  ""
a ->o?     : ""->o?  ""
a o->o?    : ""o->o? ""
a <->?     : ""<->?  ""
a o<->o?   : ""o<->o?""
a x<->x?   : ""x<->x?""
a ->>o?    : ""->>o? ""
a -\o?     : ""-\o?  ""
a -\\o?    : ""-\\\\o?""
a -/o?     : ""-/o?  ""
a -//o?    : ""-//o? ""
a x->o?    : ""x->o? ""
@enduml
|]
      parsePlantUML input `shouldBe` ( Right (PlantUML [SubjectDef (Subject Participant (AliasedName (Nq "Alice") (Nq "a")) Nothing Nothing Nothing),SubjectDef (Subject Participant (AliasedName (Nq "Bob") (Nq "b")) Nothing Nothing Nothing),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "b"))) Nothing (Just " //Long long label//")),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">?")) Nothing Nothing (Just " \"\"->?   \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">>?")) Nothing Nothing (Just " \"\"->>?  \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just "\\?")) Nothing Nothing (Just " \"\"-\\?   \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just "\\?")) Nothing Nothing (Just " \"\"-\\\\?\"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just "/?")) Nothing Nothing (Just " \"\"-/?   \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just "//?")) Nothing Nothing (Just " \"\"-//?  \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">x?")) Nothing Nothing (Just " \"\"->x?  \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr (Just "x") (Shaft (Just "-") Nothing Nothing) (Just ">?")) Nothing Nothing (Just " \"\"x->?  \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr (Just "o") (Shaft (Just "-") Nothing Nothing) (Just ">?")) Nothing Nothing (Just " \"\"o->?  \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">o?")) Nothing Nothing (Just " \"\"->o?  \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr (Just "o") (Shaft (Just "-") Nothing Nothing) (Just ">o?")) Nothing Nothing (Just " \"\"o->o? \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr (Just "<") (Shaft (Just "-") Nothing Nothing) (Just ">?")) Nothing Nothing (Just " \"\"<->?  \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr (Just "o<") (Shaft (Just "-") Nothing Nothing) (Just ">o?")) Nothing Nothing (Just " \"\"o<->o?\"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr (Just "x<") (Shaft (Just "-") Nothing Nothing) (Just ">x?")) Nothing Nothing (Just " \"\"x<->x?\"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">>o?")) Nothing Nothing (Just " \"\"->>o? \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just "\\o?")) Nothing Nothing (Just " \"\"-\\o?  \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just "\\o?")) Nothing Nothing (Just " \"\"-\\\\o?\"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just "/o?")) Nothing Nothing (Just " \"\"-/o?  \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just "//o?")) Nothing Nothing (Just " \"\"-//o? \"\"")),ArrowDef (Arrow (Just (Nq "a")) (Arr (Just "x") (Shaft (Just "-") Nothing Nothing) (Just ">o?")) Nothing Nothing (Just " \"\"x->o? \"\""))]))

    it "Specific SkinParameter. By default" $ do
      let input :: T.Text
          input = [text|
@startuml
Bob -> Alice : hello
Alice -> Bob : ok
@enduml
|]
      parsePlantUML input `shouldBe` (Right (PlantUML [ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " hello")),ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing (Just " ok"))]))

    it "lifelineStrategy solid" $ do
      let input :: T.Text
          input = [text|
@startuml
skinparam lifelineStrategy solid
Bob -> Alice : hello
Alice -> Bob : ok
@enduml
|]
      parsePlantUML input `shouldBe` (Right (PlantUML [CommandDef (SkinParameters [LifelineStrategy Solid]),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " hello")),ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing (Just " ok"))]))

    it "style strictuml" $ do
      let input :: T.Text
          input = [text|
@startuml
skinparam style strictuml
Bob -> Alice : hello
Alice -> Bob : ok
@enduml
|]
      parsePlantUML input `shouldBe` (Right (PlantUML [CommandDef (SkinParameters [Style StrictUML]),ArrowDef (Arrow (Just (Nq "Bob")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Alice"))) Nothing (Just " hello")),ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing (Just " ok"))]))

    it "Hide unlinked participant. Not hide." $ do
      let input :: T.Text
          input = [text|
@startuml
participant Alice
participant Bob
participant Carol

Alice -> Bob : hello
@enduml
|]
      parsePlantUML input `shouldBe` (Right (PlantUML [SubjectDef (Subject Participant (Name1 (Nq "Alice")) Nothing Nothing Nothing),SubjectDef (Subject Participant (Name1 (Nq "Bob")) Nothing Nothing Nothing),SubjectDef (Subject Participant (Name1 (Nq "Carol")) Nothing Nothing Nothing),ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing (Just " hello"))]))


    it "Hide unlinked participant" $ do
      let input :: T.Text
          input = [text|
@startuml
hide unlinked
participant Alice
participant Bob
participant Carol

Alice -> Bob : hello
@enduml
|]
      parsePlantUML input `shouldBe` (Right (PlantUML [CommandDef (Hide Unlinked),SubjectDef (Subject Participant (Name1 (Nq "Alice")) Nothing Nothing Nothing),SubjectDef (Subject Participant (Name1 (Nq "Bob")) Nothing Nothing Nothing),SubjectDef (Subject Participant (Name1 (Nq "Carol")) Nothing Nothing Nothing),ArrowDef (Arrow (Just (Nq "Alice")) (Arr Nothing (Shaft (Just "-") Nothing Nothing) (Just ">")) (Just (Name1 (Nq "Bob"))) Nothing (Just " hello"))]))
    it "" $ do
      let input :: T.Text
          input = [text|
@startuml
@enduml
|]
      parsePlantUML input `shouldBe` (Right (PlantUML []))
    it "" $ do
      let input :: T.Text
          input = [text|
@startuml
@enduml
|]
      parsePlantUML input `shouldBe` (Right (PlantUML []))
