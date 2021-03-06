{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.PlantUML.Parser  where

import Language.PlantUML.Types

import Debug.Trace (trace)

import Data.Maybe (isJust)
import Data.List (sortBy)
import Data.Char (isSpace)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Megaparsec hiding(parse, parseMaybe)
import qualified Text.Megaparsec as M (parse, parseMaybe)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Language.PlantUML.Types

import Language.PlantUML.ParserHelper as P

-- | Parse a whole file into a 'PlantUML' structure.
parsePlantUMLFile :: FilePath -- ^ File name of PlantUML file. This function does not care about sufix.
  -> IO (Either (ParseErrorBundle T.Text Char) (PlantUML [Declaration]))
parsePlantUMLFile p = parsePlantUML <$> T.readFile p

-- | Parse 'T.Text' into a 'PlantUML' structure.
parsePlantUML :: T.Text -- ^ Text to be parsed.
  -> Either (ParseErrorBundle T.Text Char) (PlantUML [Declaration])
parsePlantUML = P.parse plantUML "" 


plantUML :: MonadParsec Char T.Text m => m (PlantUML [Declaration])
plantUML = decls
  where
--    decls = PlantUML <$> ((reserved "@startuml") *> declarations <* (reserved "@enduml"))
    decls = PlantUML <$> between (reserved "@startuml") (reserved "@enduml") (many declaration)

declaration :: MonadParsec Char T.Text m => m Declaration
declaration = lexeme' decls
decls :: MonadParsec Char T.Text m => m Declaration
decls = (SubjectDef    <$> declSubject)
      <|> (ArrowDef    <$> declArrow)
      <|> (NotesDef    <$> declNotes)
      <|> (GroupingDef <$> declGrouping)
      <|> (CommandDef  <$> declCommand)
      <|> (BoxDef      <$> declBox)
        

dividerParser :: MonadParsec Char T.Text m => m Command
dividerParser = do
  r <- encloseDoubleChar '=' '=' 
  return $ Divider (ddrop r)

stereotype :: MonadParsec Char T.Text m => m Stereotype
stereotype = do
  r <- encloseDoubleChar '<' '>' 
  return $ Stereotype (ddrop r)

encloseDoubleChar :: MonadParsec Char T.Text m => Char -> Char -> m T.Text
encloseDoubleChar lchar rchar = do
  ls <- (\(l1, ls) -> T.pack (l1: ls)) <$> ((,) <$> char lchar <*> (many1 (char lchar)))
  rs <- doubleCharRight rchar ("", "") []
  return (T.append ls rs)

ddrop :: T.Text -> T.Text
ddrop ls = T.reverse . T.drop 2 . T.reverse $ T.drop 2 ls
  

doubleCharRight :: MonadParsec Char T.Text m => Char -> (String, T.Text) -> [T.Text] -> m T.Text
doubleCharRight ch (tmp, mark) ts = do
  rest <- lookAhead restOfLine
  let len = length ts
  if T.length rest == 0 then
    if len > 0 then
      return $ T.concat (reverse (mark : T.pack tmp : ts))
    else
      empty

    else do
      let peek = P.parse (manyTill printChar rightEnd) "" rest
      case peek of
        Right _ -> do
          r <- manyTill_ printChar rightEnd
          doubleCharRight ch r (mark : T.pack tmp  : ts)
        Left _ -> if len > 0 then
                     return $ T.concat (reverse (mark : T.pack tmp : ts))
                   else
                     return $ T.append "|" rest

  where
    rightEnd :: MonadParsec Char T.Text m => m T.Text
    rightEnd = do
      let doubleChars = T.pack [ch, ch]
      string doubleChars
      rs <- many (char ch)
      return $ T.append doubleChars (T.pack rs)
      
spacerParser :: MonadParsec Char T.Text m => m Command
spacerParser = try (Spacer <$> (string "|" *> many1 (char '|') *> optional L.decimal <* lexeme (many1 (char '|'))))
              <|> (Spacer <$> (string "||" *> lexeme (many1 (char '|')) *> pure Nothing))
  

declSubject :: MonadParsec Char T.Text m => m Subject
declSubject = choice $ map pa [("participant", Subject Participant),
                               ("actor", Subject Actor),
                               ("boundary", Subject Boundary),
                               ("control", Subject Control),
                               ("entity", Subject Entity),
                               ("database", Subject Database),
                               ("collections", Subject Collections),
                               ("queue", Subject Queue)]
  
  where
    pa :: MonadParsec Char T.Text m =>
      (T.Text, AliasedName -> Maybe Stereotype -> Maybe Order -> Maybe Color -> Subject) -> m Subject
      
    pa (txt, f) = do
      _ <- reserved txt
      a <- asName
      s <- optional (lexeme stereotype)      
      o <- optional ((reserved "order") >> lexeme L.decimal)
      c <- optional color
      return (f a s o c)


right', left', options', dash' :: [T.Text]
leftOption' = ["?", "["]
options' = ["x", "o"]
left'  = map T.pack $ sortBy order $  ["<", "<<", "\\", "\\\\", "/", "//"]
dash' = ["-"]
right' = map T.pack $ sortBy order $  [">", ">>", "\\", "\\\\", "/", "//"]
--options'
rightOption' = ["?", "]"]



order :: [a] -> [a] -> Ordering
order a b | length a == length b = EQ
order a b | length a < length b = GT
order a b | length a > length b = LT


arrow :: MonadParsec Char T.Text m => m Arr
arrow = do
  lo' <- optional (choice $ map string leftOption')
  lo <- optional (choice $ map string options')
  l <- optional (choice $ map string left')
  m <- shaft
  r <- optional (choice $ map string right')
  ro <- optional (choice $ map string options')
  ro' <- optional (choice $ map string rightOption')
  
  return $ PreArr lo' (lo <++> l) m (r <++> ro) ro'
  where
    dashes, dashes1 :: MonadParsec Char T.Text m => m T.Text    
    dashes = do
      ds <- (T.pack <$> (many (char '-')))
      if ds == "" then empty else return ds
    dashes1 = (T.pack <$> (many1 (char '-')))
    
    shaft :: MonadParsec Char T.Text m => m Shaft
    shaft =
      -- As left and right side of color should have one dash, they are separately processed.
      -- Otherwise, this parser does not consume anything and makes bigger parser falls into infinite loop.
      try (Shaft <$> (Just <$> dashes1) <*> (optional (between (string "[") (string "]") color)) <*> optional dashes)
      <|>
      try (Shaft <$> optional dashes <*> (optional (between (string "[") (string "]") color)) <*>  (Just <$> dashes1))
    norm :: Shaft -> Shaft
    norm (Shaft l m b) = Shaft (norm' l) m (norm' b)
    norm' (Just "") = Nothing
    norm' a = a

(<++>) (Just l) (Just r) = Just (T.append l r)
(<++>) (Just l) Nothing = Just l
(<++>) Nothing (Just r) = Just r
(<++>) _ _ = Nothing


declArrow :: MonadParsec Char T.Text m => m Arrow
declArrow = try(Return <$> ((reserved "return") *> optional restOfLine))
            <|> try arrowParser 
  where
    arrowParser :: MonadParsec Char T.Text m => m Arrow      
    arrowParser =
      try (Arrow <$> (Just <$> name) <*> (conv <$> lexeme arrow) <*> (Just <$> asName) <*> pure Nothing <*> ((char ':') *> (Just <$> restOfLine)))
      <|>
      try (Arrow <$> (Just <$> name) <*> (conv <$> lexeme arrow) <*> (pure Nothing) <*> pure Nothing <*> ((char ':') *> (Just <$> restOfLine)))
      <|>
      try (Arrow <$> pure Nothing<*> (conv <$> lexeme arrow) <*> (Just <$> asName) <*> pure Nothing <*> ((char ':') *> (Just <$> restOfLine)))
      <|>
      try (Arrow <$> pure Nothing<*> (conv <$> lexeme arrow) <*> (pure Nothing) <*> pure Nothing <*> ((char ':') *> (Just <$> restOfLine)))
      <|>
      try (Arrow <$> (Just <$> name) <*> (conv <$> lexeme arrow) <*> (Just . Name1 <$> name) <*>  (Just <$> color) <*> ((char ':') *> (Just <$> restOfLine)))
      <|>
      try (Arrow <$> pure Nothing <*> (conv <$> lexeme arrow) <*> (Just . Name1 <$> name) <*> (Just <$> color) <*> ((char ':') *> (Just <$> restOfLine)))
      <|>
      try (ActivationArrow <$> (Just <$> name) <*> (conv <$> lexeme arrow) <*> name <*> activityParser <*>  ((char ':') *> (Just <$> restOfLine)))
      <|>
      try (ActivationArrow <$> pure Nothing <*> (conv <$> lexeme arrow) <*> name <*> activityParser <*>  ((char ':') *> (Just <$> restOfLine)))
      
      <|>
      try (Arrow <$> (Just <$> name) <*> (conv <$> lexeme arrow) <*> (Just <$> asName) <*> pure Nothing <*> pure Nothing)
      <|>
      try (Arrow <$> (Just <$> name) <*> (conv <$> lexeme arrow) <*> (pure Nothing) <*> pure Nothing <*> pure Nothing)
      <|>
      try (Arrow <$> pure Nothing<*> (conv <$> lexeme arrow) <*> (Just <$> asName) <*> pure Nothing <*> pure Nothing)
      <|>
      try (Arrow <$> pure Nothing<*> (conv <$> lexeme arrow) <*> (pure Nothing) <*> pure Nothing <*> pure Nothing)
      <|>
      try (Arrow <$> (Just <$> name) <*> (conv <$> lexeme arrow) <*> (Just . Name1 <$> name) <*> (Just <$> color) <*> pure Nothing)
      <|>
      try (Arrow <$> pure Nothing <*> (conv <$> lexeme arrow) <*> (Just . Name1 <$> name) <*> (Just <$> color) <*> pure Nothing)
      <|>
      try (ActivationArrow <$> (Just <$> name) <*> (conv <$> lexeme arrow) <*> name <*> activityParser <*> pure Nothing)
      <|>
      try (ActivationArrow <$> pure Nothing <*> (conv <$> lexeme arrow) <*> name <*> activityParser <*> pure Nothing)


activityParser :: MonadParsec Char T.Text m => m Activity
activityParser = choice $ map (mkp pairs) pairs
  where
    pairs :: MonadParsec Char T.Text m => [(Char, m Activity)]
    pairs = [('+', Activation <$> optional color),
             ('-', pure Deactivation),
             ('*', pure Creation),
             ('!', pure Destruction)]
    mkp :: MonadParsec Char T.Text m => [(Char, m Activity)] -> (Char, m Activity) -> m Activity
    mkp ps (c, m) = char  c >> many (lexeme . choice . map (char . fst) $ ps) >> m


      
      
--conv :: A -> m Shaft
conv (PreArr lo' lo s ro ro') = Arr (lo'<++>lo) s (ro <++> ro')
conv a = a

checkArrow   (Arrow (Just _)  (PreArr (Just _) lo s ro ro')      n2       _ txt) = empty
checkArrow   (Arrow n1        (PreArr lo'      lo s ro (Just _)) (Just _) _ txt) = empty
checkArrow a@(Arrow n1     pa@(PreArr lo'      lo s ro ro')      n2       c txt) =
  return $  Arrow n1          pa                                 n2       c txt

checkArrow   (ActivationArrow (Just _) (PreArr (Just _) lo s ro ro')      n2       co txt) = empty
checkArrow   (ActivationArrow n1       (PreArr lo'      lo s ro (Just _)) n2 co txt) = empty
checkArrow a@(ActivationArrow n1     pa@(PreArr lo' lo s ro ro') n2 co txt) =
  return $ ActivationArrow    n1       pa                                 n2       co  txt
checkArrow a = return a
                                    

color ::  MonadParsec Char T.Text m => m Color
color = lexeme (try definedColor <|> try hexColor )
  where
    hexColor :: MonadParsec Char T.Text m => m Color
    hexColor = HexColor . T.pack <$> (char '#' *> (many1 hexDigitChar))
    definedColor :: MonadParsec Char T.Text m => m Color    
    definedColor = Color <$> (char '#' *> assocParser colorAssoc)

color' ::  MonadParsec Char T.Text m => m Color
color' = try definedColor <|> try hexColor 
  where
    hexColor :: MonadParsec Char T.Text m => m Color
    hexColor = HexColor . T.pack <$> (char '#' *> (many1 hexDigitChar))
    definedColor :: MonadParsec Char T.Text m => m Color    
    definedColor = Color <$> (assocParser colorAssoc)

colorAssoc :: MonadParsec Char T.Text m => [(T.Text, m DefinedColor)]
colorAssoc = mkAssoc

subjectTypeAssoc :: MonadParsec Char T.Text m => [(T.Text, m SubjectType)]
subjectTypeAssoc = mkAssoc

lifeLineOpAssoc :: MonadParsec Char T.Text m => [(T.Text, m LifeLineOp)]
lifeLineOpAssoc = mkAssoc

noteShapeAssoc' ::  [(T.Text, NoteShape)]
noteShapeAssoc' = mkAssoc'

---- Notes
declNotes ::  MonadParsec Char T.Text m => m Notes
declNotes = go
  where
    go :: MonadParsec Char T.Text m => m Notes
    go = do
      tag <- reserved "note" <|> reserved "rnote" <|> reserved "hnote" <|> reserved "ref"
      let shape =  maybe Note id  (lookup (T.toLower tag) noteShapeAssoc')
      -- do not consume spaces here
      lro <- hlexeme (try (reserved' "left") <|> try (reserved' "right" <|> try (reserved' "over")))
      case (tag, lro) of
        (_,     "left")  -> sideNote tag (NoteLeft shape)
        (_,     "right") -> sideNote tag (NoteRight shape)
        ("ref", "over")  -> overNote tag RefOver
        (_,     "over")  -> overNote tag (NoteOver shape)        
        _ -> empty
        
    sideNote :: MonadParsec Char T.Text m => T.Text -> (Maybe Name -> Maybe Color -> [T.Text] -> Notes) -> m Notes
    sideNote tag dcon = lexeme (try (caseOf tag dcon) <|> try (caseColon tag dcon) <|> try (caseRest tag dcon))


    -- Right : of, Left otherwise

    nameWithColor :: MonadParsec Char T.Text m => m (Maybe Name, Maybe Color)
    nameWithColor = do
      n <- hlexeme name'
      c <- hlexeme color
      return (Just n, Just c)
    nameOnly :: MonadParsec Char T.Text m => m (Maybe Name, Maybe Color)
    nameOnly = do
      n <- hlexeme name'
      return (Just n, Nothing)
    colorOnly :: MonadParsec Char T.Text m => m (Maybe Name, Maybe Color)
    colorOnly = do
      c <- hlexeme color
      return (Nothing, Just c)

    caseOf :: MonadParsec Char T.Text m => T.Text -> (Maybe Name -> Maybe Color -> [T.Text] -> Notes) -> m Notes
    caseOf tag dcon = do
      hlexeme (reserved' "of")
      (n, c) <- lexeme (try nameWithColor <|> try nameOnly <|> try colorOnly <|> pure (Nothing, Nothing))
      ns <- linesTill' tag []
      return $ dcon n c ns
    caseColon :: MonadParsec Char T.Text m => T.Text -> (Maybe Name -> Maybe Color -> [T.Text] -> Notes) -> m Notes
    caseColon tag dcon = do
      (n, c) <- hlexeme (try nameWithColor <|> try nameOnly <|> try colorOnly <|> pure (Nothing, Nothing))
      string ":"
      ns <- oneLine
      return $ dcon n c ns
      
    caseRest tag dcon = do
      ns <- linesTill' tag []
      return $ dcon Nothing Nothing ns
      
          
    overNote :: MonadParsec Char T.Text m => T.Text -> (Name -> Maybe Name -> Maybe Color -> [T.Text] -> Notes) -> m Notes
    overNote tag dcon = do
      first <- name
      second <- optional (lexeme (char ',') *> name)
      c <- optional color
      mark <- (string ":") <|> restOfLine'
      case mark of
        ":" -> do
          ns <- oneLine
          return $ dcon first second Nothing ns
        _ -> do
          ns <- linesTill' tag []
          return $ dcon first second c ns
        
-- Group
-- Grouping things in Declaration allows us nested structure.
declGrouping :: MonadParsec Char T.Text m => m Grouping
declGrouping = do
  groupKind <- assocParser groupingAssoc
  labels <- restOfLine -- double label temporary ignored due to difficulity
  go groupKind labels []
  where
    go :: MonadParsec Char T.Text m => GroupKind -> T.Text ->  [[Declaration]] -> m Grouping
    go k l xs = do
      let  gk = T.toLower . T.pack . show $ k
      (decls, e) <- manyTill_ declaration
        (lexeme ((try (string(endMarker gk)) <|> try (string "end") <|> try (string "else") <* restOfLine))) -- TODO
      case e of
        "else" -> do
          go k l (decls: xs)
        _ -> return $ Grouping k l (reverse (decls : xs))

declBox :: MonadParsec Char T.Text m => m Box
declBox = do
  reserved "box"
  n <- optional name
  c <- optional color
  ds <- manyTill declaration (end "box")
  return $ Box n c ds
    
doubleLabels :: MonadParsec Char T.Text m => m (T.Text, T.Text)
doubleLabels = return ("not yet", "implemented")

groupingAssoc :: MonadParsec Char T.Text m => [(T.Text, m GroupKind)]
groupingAssoc = map (\e -> (T.toLower . T.pack . show $ e, return e)) $ enumFromTo minBound maxBound

---- Commands
declCommand :: MonadParsec Char T.Text m => MonadParsec Char T.Text m => m Command
declCommand = assocParser commandAssoc
              <|> delayParser
              <|> titleParser
              <|> dividerParser
              <|> spacerParser
              <|> try skinParametersParser
              <|> try skinParamParser


hiddenItemAssoc :: MonadParsec Char T.Text m => [(T.Text, m HiddenItem)]
hiddenItemAssoc = mkAssoc

onOffAssoc :: MonadParsec Char T.Text m => [(T.Text, m OnOff)]
onOffAssoc = mkAssoc

commandAssoc :: MonadParsec Char T.Text m => [(T.Text, m Command)]
commandAssoc = [
  ("activate", Activate <$>  name <*> optional color),
  ("autoactivate", AutoActivate <$> assocParser onOffAssoc),
  ("autonumber", 
    Autonumber <$> autonumberTypeParser ),
  ("create",  LifeLine <$> pure Create <*> optional (assocParser subjectTypeAssoc) <*> name),
  ("deactivate", Deactivate <$> name),  
  ("destroy", LifeLine <$> pure Destroy <*>  pure Nothing <*> name),
  ("footer", Footer <$> optional restOfLine),  
  ("header", Header <$> optional restOfLine),
  ("newpage", NewPage <$> optional restOfLine),    
  ("hide", Hide <$> assocParser hiddenItemAssoc )]

  
titleParser :: MonadParsec Char T.Text m => m Command
titleParser = do
  t <- reserved' "title"
  r <- restOfLine
  if  r == "" then
    Title <$> T.concat <$> linesTill' "title" [r]
    else
    let res = P.parseMaybe (many space1) r in
    case res of
      Just _ -> Title <$> T.concat <$> linesTill' "title" [r]
      Nothing ->  return $ Title r

end :: MonadParsec Char T.Text m => T.Text -> m ()
end text = (reserved' "end" >> (char ' ') >> reserved text >> pure ())
  <|> (reserved' (T.append "end" text) >> pure())

linesTill' ::MonadParsec Char T.Text m => T.Text -> [T.Text]  -> m [T.Text]
linesTill' txt xs = do
  r <- restOfLine
  let res =  P.parseMaybe (end txt) r
  case res of
    Just _ -> do
      return $ reverse xs
    Nothing -> linesTill' txt (r : xs)

  
autonumberTypeParser :: MonadParsec Char T.Text m => m AutonumberType
autonumberTypeParser =  (reserved "stop" >> pure Stop) 
                        <|> (Resume <$> (reserved "resume" *> optional (lexeme L.decimal)) <*> optional quotedName)
                        <|> (Start <$>  optional (lexeme L.decimal) <*> optional (lexeme L.decimal) <*> optional quotedName)



delayParser :: MonadParsec Char T.Text m => m Command
delayParser = do
  reservedSymbol "..."
  ls <- lookAhead restOfLine
  let txt = P.parseMaybe ptxt ls
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
skinParamParser = SkinParameters . (:[]) <$> (lexeme (reserved "skinparam") *> lexeme (assocParser skinParamAssoc))


skinParametersParser :: MonadParsec Char T.Text m => m Command
skinParametersParser = 
  SkinParameters <$> (reserved "skinparam" *> reserved "sequence" *> 
                       between (lexeme' (char '{')) (lexeme' (char '}')) (many (lexeme' (assocParser skinParamAssoc))))


skinParamAssoc :: MonadParsec Char T.Text m => [(T.Text, m SkinParam)]
skinParamAssoc = [
    ("arrowColor", ArrowColor <$> lexeme color'),
    ("backgroundColor", BackgroundColor <$> lexeme color'),
    ("boxPadding", BoxPadding <$> lexeme L.decimal),    
    ("guillemet", Guillemet <$> assocParser boolAssoc),
    ("handwritten", Handwritten <$> assocParser boolAssoc),
    ("lifelineStrategy", LifelineStrategy <$> assocParser lifelineStrategyTypeAssoc),
    ("lifeLineBorderColor", LifeLineBorderColor <$> lexeme color'),
    ("lifeLineBackgroundColor", LifeLineBackgroundColor <$> lexeme color),
    ("maxMessageSize", MaxMessageSize <$> lexeme L.decimal),
    ("participantPadding", ParticipantPadding <$> lexeme L.decimal),    
    ("responseMessageBelowArrow", ResponseMessageBelowArrow <$> assocParser boolAssoc),    
    ("roundCorner", RoundCorner <$> lexeme L.decimal),    
    ("sequenceArrowThickness",  SequenceArrowThickness <$> lexeme L.decimal),
    ("sequenceParticipant", SequenceParticipant <$> assocParser sequenceParticipantTypeAssoc),    
    ("style", Style <$> assocParser styleTypeAssoc),


    ("actorBackgroundColor", ActorBackgroundColor <$> lexeme color'),
    ("actorBorderColor", ActorBorderColor <$> lexeme color'),      
    ("actorFontColor", ActorFontColor <$> lexeme color'),
    ("actorFontSize", ActorFontSize <$> lexeme L.decimal),
    ("actorFontName", ActorFontName <$> (T.pack <$> lexeme (many1 letterChar))),


    ("participantBackgroundColor", ParticipantBackgroundColor <$> lexeme color'),
    ("participantBorderColor", ParticipantBorderColor <$> lexeme color'),
    ("participantFontColor", ParticipantFontColor <$> lexeme color'),    
    ("participantFontName", ParticipantFontName <$> (T.pack <$> lexeme (many1 letterChar))),
    ("participantFontSize", ParticipantFontSize <$> lexeme L.decimal)
  ]


name' :: MonadParsec Char T.Text m => m Name
name' =  (Q <$> quotedName) <|> (Nq <$> nonQuotedName)
name :: MonadParsec Char T.Text m => m Name
name = lexeme name'

reservedAs :: MonadParsec Char T.Text m => m Name
--reservedAs = optional (Nq <$> (lexeme (reserved "as") *> lexeme nonQuotedName))
reservedAs = do
  reserved "as"
  name
  


asName ::  MonadParsec Char T.Text m => m AliasedName
asName = do
  n1 <- name
  foundAs <- optional reservedAs
  case (n1, foundAs) of
    (Q _, Just m2@(Q _))   -> empty
    (Q _, Just m2@(Nq _))  -> return $ AliasedName n1 m2
    (Q _, Nothing)         -> return $ Name1 n1
    (Nq _, Just m2@(Q _))  -> return $ AliasedName n1 m2
    (Nq _, Just m2@(Nq _)) -> return $ AliasedName n1 m2
    (Nq _, Nothing)        -> return $ Name1 n1

