{-# language GADTSyntax, PatternSynonyms, OverloadedStrings #-}
module Language.PlantUML.Types where

import Data.String (IsString(..))
import qualified Data.Text as T

data PlantUML where
  PlantUML :: [Declaration] -> PlantUML
  deriving (Eq, Show)


newtype Name = Name T.Text
  deriving (Eq, Show)
instance IsString Name where
  fromString = Name . T.pack

newtype Alias = Alias T.Text
  deriving (Eq, Show)
instance IsString Alias where
  fromString = Alias . T.pack



data Subject where
  Participant :: Name -> Maybe Alias -> Subject
  Actor       :: Name -> Maybe Alias -> Subject
  Boundary    :: Name -> Maybe Alias -> Subject
  Control     :: Name -> Maybe Alias -> Subject
  Entity      :: Name -> Maybe Alias -> Subject
  Database    :: Name -> Maybe Alias -> Subject
  Collections :: Name -> Maybe Alias -> Subject
  Queue       :: Name -> Maybe Alias -> Subject
  deriving (Eq, Show)


data ArrowEnd = ArrowHead | ArrowTail
  deriving (Eq, Show)

data DefinedColor = Red | Blue | Yellow | Black | White
  deriving (Eq, Show, Enum, Bounded)

data Color where
  Color :: DefinedColor -> Color
  HexColor :: T.Text -> Color  
  deriving (Eq, Show)

data Arrow where
  Arrow ::  Maybe T.Text -> T.Text -> Maybe T.Text -> Maybe [T.Text] -> Arrow
  deriving (Eq, Show)

data Notes where
  NoteLeft  :: Maybe Name -> [T.Text] -> Notes
  NoteRight :: Maybe Name -> [T.Text] -> Notes
  NoteOver  :: Name -> Maybe Name -> [T.Text] -> Notes
  deriving (Eq, Show)

data Declaration where
  ArrowDef      :: Arrow      -> Declaration
  SubjectDef    :: Subject    -> Declaration
  NotesDef      :: Notes      -> Declaration
  GroupingDef   :: Grouping   -> Declaration
  CommandDef    :: Command    -> Declaration
  deriving (Eq, Show)


--data Box where
--  Box :: T.Text -> Maybe Color -> [Declaration] -> Box
--  deriving (Eq, Show)  


-- ^ There are multiple ways of  grouping elements used in sequence diagrams.
-- The data type deals the following key words.
-- alt/else
-- break
-- critical
-- group
-- loop
-- opt
-- par

-- box
-- box is a special key word in the following sense:
-- box does not accepts else element but all other constructions accepts "else".
-- box draws vertical box and all other constructions are drawn horizontally.
-- As a result Box is defined specifically
data GroupKind = Alt | Opt | Loop | Par | Break | Critical | Group
  deriving(Eq, Show, Enum, Bounded)

data Grouping where
  -- Horizontal grouping provide optional grouping using else keyword,
  -- multiple Declaration groups are allowed.
  Grouping :: GroupKind -> [T.Text] -> [[Declaration]] -> Grouping
  Box :: [Declaration] -> [T.Text] -> Grouping
  deriving (Eq, Show)

data HiddenItem = FootBox | Unlinked
  deriving (Eq, Show, Enum, Bounded)

data Command where
  Activate :: Name -> Command  -- implemented
  Autonumber :: Maybe Integer -> Maybe Integer -> Maybe Integer -> Command -- implemented
  AutonumberStop :: Command
  AutonumberResume :: Maybe T.Text -> Command
  Deactivate :: Name -> Command
  Hide :: HiddenItem -> Command
  NewPage :: T.Text -> Command
  Title :: T.Text -> Command
  Divider :: T.Text -> Command
  VSpace :: Command
  deriving (Eq, Show)


data SkinParameter where
  ResponseMessageBelowArrow :: Bool -> SkinParameter
  MaxMessageSize :: Int -> SkinParameter
  Guillment :: Bool -> SkinParameter
  SequenceArrowThickness :: Int -> SkinParameter
  RoundCorner :: Int -> SkinParameter
  SequenceParticipant :: T.Text -> SkinParameter
  BackgroundColor :: Color -> SkinParameter
  Handwritten :: Bool -> SkinParameter
  ParticipantPadding :: Int -> SkinParameter
  BoxPadding :: Int -> SkinParameter
  LifelineStrategy :: T.Text -> SkinParameter
  Style :: T.Text -> SkinParameter

  -- Sequence diagram specific skin parameter
  ArrowColor :: Color -> SkinParameter
  ActorBorderColor :: Color -> SkinParameter
  LifeLineBorderColor :: Color -> SkinParameter
  LifeLineBackgroundColor :: Color -> SkinParameter

  ParticipantBorderColor :: Color -> SkinParameter
  ParticipantBackgroundColor :: Color -> SkinParameter
  ParticipantFontName :: T.Text -> SkinParameter
  ParticipantFontSize :: Int -> SkinParameter
  ParticipantFontColor :: Color -> SkinParameter

  ActorBackgroundColor :: Color -> SkinParameter
  ActorFontColor :: Color -> SkinParameter
  ActorFontSize :: Int -> SkinParameter
  ActorFontName :: T.Text -> SkinParameter
  
