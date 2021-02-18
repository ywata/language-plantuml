{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.PlantUML.Parser  where

import Language.PlantUML.Types


import Data.List (sortBy)
import Data.Char (isSpace)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Language.PlantUML.Types

import Language.PlantUML.ParserHelper as P

-- | Parse a whole file into a 'PlantUML' structure.
parsePlantUMLFile :: FilePath -> IO (Either (ParseErrorBundle T.Text Char) PlantUML)
parsePlantUMLFile p = parsePlantUML <$> T.readFile p

-- | Parse 'T.Text' into a 'PlantUML' structure.
parsePlantUML :: T.Text -> Either (ParseErrorBundle T.Text Char) PlantUML
parsePlantUML = parse plantUML ""


plantUML :: MonadParsec Char T.Text m => m PlantUML
plantUML = decls
  where
    decls = PlantUML <$> ((lexeme (reserved "@startuml")) >> declarations <* (lexeme (reserved "@enduml")))

declarations :: MonadParsec Char T.Text m => m [Declaration]
declarations = many declaration

declaration :: MonadParsec Char T.Text m => m Declaration
declaration = lexeme decls
decls :: MonadParsec Char T.Text m => m Declaration
decls = (SubjectDef <$> declSubject)
      <|> (ArrowDef <$> declArrow)
      <|> (NotesDef <$> declNotes)
      <|> (GroupingDef <$> declGrouping)
      <|> (CommandDef <$> declCommand)
        

stereotype :: (MonadParsec Char T.Text m) => m a -> m (Stereotype [a])
stereotype p = do
  f <- string "<<"
  g <- manyTill p (notFollowedBy (string ">>"))
  return $ Stereotype g


  

declSubject :: MonadParsec Char T.Text m => m Subject
declSubject = alt $ map pa [(Participant, "participant")
                , (Actor, "actor")
                , (Boundary, "boundary")
                , (Control, "control")
                , (Entity, "entity")
                , (Database, "database")
                , (Collections, "collections")
                , (Queue, "queue")]
                                                           
  where
    alt [] = empty
    alt (x : xs) = x <|> alt xs
    pa (f, txt) = do
      n <- Name <$> ((lexeme $ reserved txt) *> lexeme name)
      a <- reservedAs
      o <- optional (lexeme (reserved "order") >> lexeme L.decimal)
      c <- optional (lexeme color)
      return (f n a o c)


--- Arrows
right, left, options, arrows, dashes :: [String]
right = ["", ">", ">>", "\\", "\\\\", "/", "//"]
left =  ["", "<", "<<", "\\", "\\\\", "/", "//"]
options = ["", "x", "o"]
dashes = ["-", "--"]

-- sort by declising length to avoid unwanted match. (if not sorted, --> can match against -->o)
arrows = sortBy order $ map (\(a,b,c,d,e) -> a ++ b++ c++ d++e) $
           filter f [(lopt, l,  dash, r, ropt) | dash <- dashes, r <- right, l <- left, ropt <- options, lopt <- options]
  where
    order :: [a] -> [a] -> Ordering
    order a b | length a == length b = EQ
    order a b | length a < length b = GT
    order a b | length a > length b = LT
    -- the following pattern is not allowed for arrow
    f ("", "", _, "", "") = False
    f ("o", "", _, "", "") = False
    f ("", "", _, "", "o") = False    
    f ("o", "", _, "", "o") = False
    f _ = True

parrows :: MonadParsec Char T.Text m => [m T.Text]
parrows = map string $ map T.pack arrows
declArrow :: MonadParsec Char T.Text m => m Arrow
declArrow = try (Arrow
            <$> optional (lexeme name)
            <*> (lexeme (choice parrows))
            <*> optional (lexeme name)
            <*> optional (lexeme $ (char ':') *> description))
            <|>
            try (Return <$> (lexeme $reserved "return" *> restOfLine))


description:: MonadParsec Char T.Text m => m [T.Text]
description = restOfLine

color ::  MonadParsec Char T.Text m => m Color
color = try definedColor <|> try hexColor
  where
    hexColor :: MonadParsec Char T.Text m => m Color
    hexColor = HexColor . T.pack <$> (char '#' *> (many1 hexDigitChar))
    definedColor :: MonadParsec Char T.Text m => m Color    
    definedColor = Color <$> (char '#' *> assocParser colorAssoc)

colorAssoc :: MonadParsec Char T.Text m => [(T.Text, m DefinedColor)]
colorAssoc = mkAssoc
    

reservedAs :: MonadParsec Char T.Text m => m (Maybe Alias)
reservedAs = optional (Alias <$> (lexeme (reserved "as") *> lexeme nonQuotedName))


---- Notes
declNotes ::  MonadParsec Char T.Text m => m Notes
declNotes = go
  where
    go :: MonadParsec Char T.Text m => m Notes
    go = do
      lexeme $ reserved "note"
      lro <- try (lexeme $ reserved "left") <|> (lexeme $ reserved "right" <|> (lexeme $ reserved "over"))
      case lro of
        "left"  -> sideNote NoteLeft
        "right" -> sideNote NoteRight
        "over" -> overNote NoteOver
        _ -> empty
    sideNote :: MonadParsec Char T.Text m => (Maybe Name -> [T.Text] -> Notes) -> m Notes
    sideNote dcon = do
      sm <- lexeme (string ":") <|> lexeme (string "of")
      case sm of
        ":" -> do
          note <- oneLine
          return $ dcon Nothing note
        "of" -> do
          name <- optional (Name <$> lexeme name)
          note <- multiLine (isEndWith "note")
          return $ dcon name note
    overNote :: MonadParsec Char T.Text m => (Name -> Maybe Name -> [T.Text] -> Notes) -> m Notes
    overNote dcon = do
      first <- Name <$> lexeme name
      second <- optional (Name <$> (lexeme (char ',') *> lexeme name))
      mark <- (string ":") <|> (T.pack <$> manyTill printChar endOfLine)
      case mark of
        ":" -> do
          note <- oneLine
          return $ dcon first second note
        _ -> do
          note <- multiLine (isEndWith "note")
          return $ dcon first second note
    
       
-- Group
-- Grouping things in Declaration allows us nested structure.
declGrouping :: MonadParsec Char T.Text m => m Grouping
declGrouping = do
  groupKind <- assocParser groupingAssoc
  labels <- restOfLine -- double label temporary ignored due to difficulity
  go groupKind labels []
  where

    go :: MonadParsec Char T.Text m => GroupKind -> [T.Text] ->  [[Declaration]] -> m Grouping
    go k l xs = do
      let  gk = T.toLower . T.pack . show $ k
      (decls, e) <- manyTill_ declaration
        (spaceConsumer *> (string(endMarker gk) <|> string "else") <* spaceConsumer)
      case e of
        "else" -> go k l (decls: xs)
        _ -> return $ Grouping k l (reverse (decls : xs))
    
doubleLabels :: MonadParsec Char T.Text m => m (T.Text, T.Text)
doubleLabels = return ("not yet", "implemented")

groupingAssoc :: MonadParsec Char T.Text m => [(T.Text, m GroupKind)]
groupingAssoc = map (\e -> (T.toLower . T.pack . show $ e, return e)) $ enumFromTo minBound maxBound

---- Commands
declCommand :: MonadParsec Char T.Text m => MonadParsec Char T.Text m => m Command
declCommand = assocParser commandAssoc <|> delayParser <|> skinParamParser

hiddenItemAssoc :: MonadParsec Char T.Text m => [(T.Text, m HiddenItem)]
hiddenItemAssoc = mkAssoc

onOffAssoc :: MonadParsec Char T.Text m => [(T.Text, m OnOff)]
onOffAssoc = mkAssoc

commandAssoc :: MonadParsec Char T.Text m => [(T.Text, m Command)]
commandAssoc = [
  ("activate", Activate <$> (Name <$> name) <*> optional (lexeme color)),
  ("autoactivate", AutoActivate <$> assocParser onOffAssoc),
  ("autonumber", 
    Autonumber <$> optional (lexeme L.decimal) <*>  optional (lexeme L.decimal) <*>  optional (lexeme L.decimal) ),
  ("create", Create <$> (Name <$> name)),
  ("destroy", Destroy <$> (Name <$> name)),
  ("hide", Hide <$> assocParser hiddenItemAssoc ),

  ("deactivate", Deactivate <$> (Name <$> name)),
  ("hide", Hide <$> assocParser hiddenItemAssoc ),
  ("title", Title <$> restOfLine)
  ]

delayParser :: MonadParsec Char T.Text m => m Command
delayParser = do
  reservedSymbol "..."
  ls <- lookAhead restOfLine
  let txt = parseMaybe ptxt (T.concat ls)
  case txt of
    Just t -> do
      _ <- restOfLine
      return $ Delay (Just t)
    _ -> return $ Delay Nothing
  where
    ptxt :: MonadParsec Char T.Text m => m T.Text
    ptxt = T.pack <$> manyTill printChar (many1 (reservedSymbol "..."))
    

boolAssoc :: MonadParsec Char T.Text m => [(T.Text, m Bool)]
boolAssoc = mkAssoc
lifelineStrategyTypeAssoc :: MonadParsec Char T.Text m => [(T.Text, m LifelineStrategyType)]
lifelineStrategyTypeAssoc = mkAssoc
styleTypeAssoc :: MonadParsec Char T.Text m => [(T.Text, m StyleType)]
styleTypeAssoc = mkAssoc
sequenceParticipantTypeAssoc :: MonadParsec Char T.Text m => [(T.Text, m SequenceParticipantType)]
sequenceParticipantTypeAssoc = mkAssoc

skinParamParser :: MonadParsec Char T.Text m => m Command
skinParamParser = SkinParameter . (:[]) <$> (lexeme (reserved "skinparam") *> lexeme (assocParser skinParamAssoc))

skinParamAssoc :: MonadParsec Char T.Text m => [(T.Text, m SkinParam)]
skinParamAssoc = [
    ("responseMessageBelowArrow", ResponseMessageBelowArrow <$> assocParser boolAssoc),
    ("maxMessageSize", MaxMessageSize <$> lexeme L.decimal),
    ("guillment", Guillment <$> assocParser boolAssoc),
    ("sequenceArrowThickness",  SequenceArrowThickness <$> lexeme L.decimal),
    ("roundCorner", RoundCorner <$> lexeme L.decimal),
    ("sequenceParticipant", SequenceParticipant <$> assocParser sequenceParticipantTypeAssoc),
    ("backgroundColor", BackgroundColor <$> lexeme color),
    ("handwritten", Handwritten <$> assocParser boolAssoc),
    ("participantPadding", ParticipantPadding <$> lexeme L.decimal),
    ("boxPadding", BoxPadding <$> lexeme L.decimal),
    ("lifelineStrategy", LifelineStrategy <$> assocParser lifelineStrategyTypeAssoc),
    ("style", Style <$> assocParser styleTypeAssoc),
    ("arrowColor", ArrowColor <$> lexeme color),
    ("actorBorderColor", ActorBorderColor <$> lexeme color),
    ("lifeLineBorderColor", LifeLineBorderColor <$> lexeme color),
    ("lifeLineBackgroundColor", LifeLineBackgroundColor <$> lexeme color),
    ("participantBorderColor", ParticipantBorderColor <$> lexeme color),
    ("participantBackgroundColor", ParticipantBackgroundColor <$> lexeme color),
--    ("participantFontName", ParticipantFontName <$> lexame string),
    ("participantFontSize", ParticipantFontSize <$> lexeme L.decimal),
    ("participantFontColor", ParticipantFontColor <$> lexeme color),
    ("actorBackgroundColor", ActorBackgroundColor <$> lexeme color),
    ("actorFontColor", ActorFontColor <$> lexeme color),
    ("actorFontSize", ActorFontSize <$> lexeme L.decimal)
--    ("actorFontName", ActorFontName <$> lexeme color)
  ]




