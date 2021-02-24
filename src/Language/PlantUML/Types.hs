{-# language GADTSyntax, PatternSynonyms, OverloadedStrings #-}
module Language.PlantUML.Types where

import Data.String (IsString(..))
import qualified Data.Text as T

data PlantUML where
  PlantUML :: [Declaration] -> PlantUML
  deriving (Eq, Show)

data Name = Q T.Text | Nq T.Text
  deriving (Eq, Show)

data AliasedName = Name1 Name
                 | AliasedName Name Name
                 | NameColor Name Color
--                 | NoName
  deriving (Eq, Show)
type Order = Integer

data SubjectType = Participant | Actor | Boundary | Control | Entity | Database | Collections | Queue
  deriving (Eq, Show, Enum, Bounded)

data Subject where
  Subject     :: SubjectType -> AliasedName -> Maybe Stereotype -> Maybe Order -> Maybe Color -> Subject
{-  Participant :: AliasedName -> Maybe Stereotype -> Maybe Order -> Maybe Color -> Subject
  Actor       :: AliasedName -> Maybe Stereotype -> Maybe Order -> Maybe Color -> Subject
  Boundary    :: AliasedName -> Maybe Stereotype -> Maybe Order -> Maybe Color -> Subject
  Control     :: AliasedName -> Maybe Stereotype -> Maybe Order -> Maybe Color -> Subject
  Entity      :: AliasedName -> Maybe Stereotype -> Maybe Order -> Maybe Color -> Subject
  Database    :: AliasedName -> Maybe Stereotype -> Maybe Order -> Maybe Color -> Subject
  Collections :: AliasedName -> Maybe Stereotype -> Maybe Order -> Maybe Color -> Subject
  Queue       :: AliasedName -> Maybe Stereotype -> Maybe Order -> Maybe Color -> Subject
-}
  deriving (Eq, Show)

data Shaft = Shaft (Maybe T.Text) (Maybe Color) (Maybe T.Text)
  deriving(Show, Eq)

data Arr = Arr (Maybe T.Text) Shaft (Maybe T.Text)
           | PreArr (Maybe T.Text) (Maybe T.Text) Shaft (Maybe T.Text) (Maybe T.Text)
  deriving(Show, Eq)

data DefinedColor = Aqua | Black | Blue | DarkSalmon | DeepSkyBlue | DodgerBlue | LightBlue | Red  | Yellow |White
  deriving (Eq, Show, Enum, Bounded)

data Color where
  Color :: DefinedColor -> Color
  HexColor :: T.Text -> Color  
  deriving (Eq, Show)

data Arrow where
  Arrow ::  Maybe Name -> Arr  -> Maybe AliasedName -> Maybe T.Text -> Arrow  
  Return :: Maybe T.Text -> Arrow
  deriving (Eq, Show)

data NoteShape = Note |RNote | HNote
  deriving (Eq, Show, Enum, Bounded)

data Notes where
  NoteLeft  :: NoteShape ->         Maybe Name -> Maybe Color -> [T.Text] -> Notes
  NoteRight :: NoteShape ->         Maybe Name -> Maybe Color -> [T.Text] -> Notes
  NoteOver  :: NoteShape -> Name -> Maybe Name -> Maybe Color -> [T.Text] -> Notes
  RefOver   :: Name ->              Maybe Name -> Maybe Color -> [T.Text] -> Notes   -- Color is not used
  deriving (Eq, Show)

data Declaration where
  ArrowDef      :: Arrow      -> Declaration
  SubjectDef    :: Subject    -> Declaration
  NotesDef      :: Notes      -> Declaration
  GroupingDef   :: Grouping   -> Declaration
  CommandDef    :: Command    -> Declaration
  BoxDef        :: Box        -> Declaration
  deriving (Eq, Show)

data Stereotype where
  Stereotype :: T.Text -> Stereotype
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
  Grouping :: GroupKind -> T.Text -> [[Declaration]] -> Grouping
  deriving (Eq, Show)

data Box where
  Box :: Maybe Name -> Maybe Color -> [Declaration] -> Box
  deriving (Eq, Show)

data HiddenItem = FootBox | Unlinked
  deriving (Eq, Show, Enum, Bounded)

data OnOff = On | Off
  deriving (Eq, Show, Enum, Bounded)

data AutonumberType = Start (Maybe Integer) (Maybe Integer) (Maybe T.Text)
                    | Resume (Maybe Integer) (Maybe T.Text)
                    | Stop 
  deriving (Eq, Show)

data LifeLineOp = Create | Destroy
  deriving (Eq, Show, Enum, Bounded)

data Command where
  Activate :: Name -> Maybe Color -> Command  -- implemented
  AutoActivate :: OnOff -> Command
  Autonumber :: AutonumberType -> Command -- implemented
--  Create :: Name -> Command
  Deactivate :: Name -> Command
  Delay :: Maybe T.Text -> Command
--  Destroy :: Name -> Command
  Header :: Maybe T.Text -> Command
  Footer :: Maybe T.Text -> Command
  Hide :: HiddenItem -> Command
  LifeLine :: LifeLineOp -> Maybe SubjectType -> Name -> Command
  NewPage :: Maybe T.Text -> Command
  Title :: T.Text -> Command
  Divider :: T.Text -> Command
  SkinParameters :: [SkinParam] -> Command
  Space :: Maybe Integer -> Command
  VSpace :: Command
  deriving (Eq, Show)


data LifelineStrategyType = Solid | Dash
  deriving (Eq, Show, Enum, Bounded)
data StyleType = StrictUML
  deriving (Eq, Show, Enum, Bounded)
data SequenceParticipantType = Underline
  deriving (Eq, Show, Enum, Bounded)


data SkinParam where
  ResponseMessageBelowArrow :: Bool -> SkinParam
  MaxMessageSize :: Int -> SkinParam
  Guillemet :: Bool -> SkinParam
  SequenceArrowThickness :: Int -> SkinParam
  RoundCorner :: Int -> SkinParam
  SequenceParticipant :: SequenceParticipantType -> SkinParam
  BackgroundColor :: Color -> SkinParam
  Handwritten :: Bool -> SkinParam
  ParticipantPadding :: Int -> SkinParam
  BoxPadding :: Int -> SkinParam
  LifelineStrategy :: LifelineStrategyType -> SkinParam
  Style :: StyleType -> SkinParam

  -- Sequence diagram specific skin parameter
  ArrowColor :: Color -> SkinParam
  ActorBorderColor :: Color -> SkinParam
  LifeLineBorderColor :: Color -> SkinParam
  LifeLineBackgroundColor :: Color -> SkinParam

  ParticipantBorderColor :: Color -> SkinParam
  ParticipantBackgroundColor :: Color -> SkinParam
  ParticipantFontName :: T.Text -> SkinParam
  ParticipantFontSize :: Int -> SkinParam
  ParticipantFontColor :: Color -> SkinParam

  ActorBackgroundColor :: Color -> SkinParam
  ActorFontColor :: Color -> SkinParam
  ActorFontSize :: Int -> SkinParam
  ActorFontName :: T.Text -> SkinParam
  deriving (Eq, Show)
