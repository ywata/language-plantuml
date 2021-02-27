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
  deriving (Eq, Show)
type Order = Integer

data SubjectType = Participant | Actor | Boundary | Control | Entity | Database | Collections | Queue
  deriving (Eq, Show, Enum, Bounded)

data Subject where
  Subject     :: SubjectType -> AliasedName -> Maybe Stereotype -> Maybe Order -> Maybe Color -> Subject
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

data Activity = Creation |Destruction | Activation (Maybe Color) | Deactivation
  deriving (Eq, Show)


data Arrow where
  Arrow  ::  Maybe Name -> Arr  -> Maybe AliasedName -> {- Color -> -} Maybe T.Text -> Arrow
  Arrow2  ::  Maybe Name -> Arr  -> Maybe AliasedName -> Color -> Maybe T.Text -> Arrow  
  ActivationArrow :: Maybe Name -> Arr -> Name -> Activity -> Maybe T.Text -> Arrow    
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
  AnchorDef     :: Anchor     -> Declaration
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

data Anchor where
  Anchor :: T.Text -> [Declaration] -> Anchor
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
  Deactivate :: Name -> Command
  Delay :: Maybe T.Text -> Command
  Header :: Maybe T.Text -> Command
  Footer :: Maybe T.Text -> Command
  Hide :: HiddenItem -> Command
  LifeLine :: LifeLineOp -> Maybe SubjectType -> Name -> Command
  NewPage :: Maybe T.Text -> Command
  Title :: T.Text -> Command
  Divider :: T.Text -> Command
  SkinParameters :: [SkinParam] -> Command
  Spacer :: Maybe Integer -> Command
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

  LifeLineBorderColor :: Color -> SkinParam
  LifeLineBackgroundColor :: Color -> SkinParam

  ActorBorderColor :: Color -> SkinParam
  ActorBackgroundColor :: Color -> SkinParam
  ActorFontColor :: Color -> SkinParam
  ActorFontSize :: Int -> SkinParam
  ActorFontName :: T.Text -> SkinParam

  ParticipantBorderColor :: Color -> SkinParam
  ParticipantBackgroundColor :: Color -> SkinParam
  ParticipantFontName :: T.Text -> SkinParam
  ParticipantFontSize :: Int -> SkinParam
  ParticipantFontColor :: Color -> SkinParam
  
  deriving (Eq, Show)
