--{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE GADTs #-}
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

-- | Parse a whole file into a 'PlantUML' structure.
parsePlantUMLFile :: FilePath -> IO (Either (ParseErrorBundle T.Text Char) PlantUML)
parsePlantUMLFile p = parsePlantUML <$> T.readFile p

-- | Parse 'T.Text' into a 'PlantUML' structure.
parsePlantUML :: T.Text -> Either (ParseErrorBundle T.Text Char) PlantUML
parsePlantUML = parse plantUML ""



spaceConsumer :: MonadParsec Char T.Text m => m ()
spaceConsumer = L.space space1 (L.skipLineComment "'") (L.skipBlockComment "/'" "'/")

lexeme :: MonadParsec Char T.Text m => m a -> m a
lexeme  = L.lexeme spaceConsumer
symbol :: MonadParsec Char T.Text m => T.Text -> m T.Text
symbol = L.symbol spaceConsumer

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
--      <|> (GroupingDef <$> declGrouping)



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
      n <- Name <$> ((lexeme $ reserved txt) *> name)
      a <- (spaceConsumer *> reservedAs) <|> pure Nothing
      return (f n a)

-- elements of arrows witout color specifier
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
declArrow = Arrow
            <$> optional (lexeme name)
            <*> (lexeme (choice parrows))
            <*> optional (lexeme name)
            <*> optional (lexeme $ (char ':') *> description)

description:: MonadParsec Char T.Text m => m [T.Text]
description = restOfLine

----
assocParser :: (Enum a, MonadParsec Char T.Text m) => [(T.Text, a)] -> m a
assocParser = choice . map pairParser


pairParser :: MonadParsec Char T.Text m => (T.Text, a) -> m a
pairParser (txt, a) = id <$> lexeme (reserved txt) *> return a


color ::  MonadParsec Char T.Text m => m Color
color = lexeme $ try definedColor <|> try hexColor
  where
    hexColor :: MonadParsec Char T.Text m => m Color
    hexColor = HexColor . T.pack <$> (char '#' *> (many1 hexDigitChar))
    definedColor :: MonadParsec Char T.Text m => m Color    
    definedColor = Color <$> (char '#' *> assocParser colormap)
    

reservedAs :: MonadParsec Char T.Text m => m (Maybe Alias)
reservedAs = optional (Alias <$> (lexeme (reserved "as") *> nonQuotedName))


---- Notes
endOfLine :: MonadParsec Char T.Text m => m T.Text
endOfLine = crlf <|> (T.pack . (:[]) <$> newline)
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
    
isEndWith endKey str = case res of
   Just _ -> True
   _ -> False
   where
     res = parseMaybe (spaceConsumer *> string (endMarker endKey) *> spaceConsumer) str

endMarker :: T.Text -> T.Text
endMarker endKey = T.append  "end " endKey
       
-- Group
-- Grouping things in Declaration allows us nested structure.
declGrouping :: MonadParsec Char T.Text m => m Grouping
declGrouping = do
  groupKind <- assocParser groupingMap
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

----
--data Test where
--  T1 :: Test
--  T2 :: T.Text -> Test

--commandParser :: MonadParsec Char T.Text m => forall a b. [(a -> b, m a)]
--commandParser = [(T1, _), (T2, _)]

----
many1:: MonadParsec Char T.Text m => m a -> m [a]
many1 p = do
  c <- p
  cs <- many p
  return (c : cs)

--- Keyword name of elements of diagram
reserved :: MonadParsec Char T.Text m => T.Text -> m T.Text
reserved txt = do
  n <- lookAhead ident
  if n == txt then ident else empty

ident :: MonadParsec Char T.Text m => m T.Text
ident = (\h t -> T.pack (h:t))
        <$> (letterChar <|> char '@')
        <*> many (alphaNumChar <|> char '_')

nonQuotedName :: MonadParsec Char T.Text m => m T.Text
nonQuotedName = T.pack <$> many1 letterChar

quotedName :: MonadParsec Char T.Text m => m T.Text
quotedName = T.pack <$> (char '"' >> manyTill L.charLiteral (char '"'))

name :: MonadParsec Char T.Text m => m T.Text
name = quotedName <|> nonQuotedName



----
oneLine :: MonadParsec Char T.Text m => m [T.Text]
oneLine = ((:[]) . T.pack) <$> manyTill printChar endOfLine

---- restOfLine treates continuation line or virtual one line
restOfLine :: MonadParsec Char T.Text m => m [T.Text]
restOfLine = restOfLine' []
  where
    restOfLine' :: MonadParsec Char T.Text m => [T.Text] -> m [T.Text]
    restOfLine' xs = do
      l <- T.pack <$> manyTill printChar endOfLine
      if isContinueLine l then
        restOfLine' (l:xs)
        else
        return $ reverse (l : xs)
isContinueLine :: T.Text -> Bool
isContinueLine = T.isSuffixOf "\\"


multiLine :: MonadParsec Char T.Text m => (T.Text -> Bool) -> m [T.Text]
multiLine = multiLine' []

multiLine' :: MonadParsec Char T.Text m => [T.Text] -> (T.Text ->Bool) -> m [T.Text]
multiLine' xs prep = do
  l <- T.pack <$> manyTill printChar endOfLine
  if prep l then
    return (reverse xs)
  else
    multiLine' (l:xs) prep

--miscParsers :: MonadParsec Char T.Text m => [(String, m Command)]
--miscParsers = [(Autonumber, triple (optional Nothing(lexeme L.


