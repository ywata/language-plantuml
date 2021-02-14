{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.PlantUML.ParserHelper where
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T

spaceConsumer :: MonadParsec Char T.Text m => m ()
spaceConsumer = L.space space1 (L.skipLineComment "'") (L.skipBlockComment "/'" "'/")

lexeme :: MonadParsec Char T.Text m => m a -> m a
lexeme  = L.lexeme spaceConsumer

mkAssoc :: (Show a, Enum a, Bounded a, MonadParsec Char T.Text m) => [(T.Text, m a)]
mkAssoc = map (\e -> (T.toLower . T.pack . show $ e, return e)) $ enumFromTo minBound maxBound


----
many1:: MonadParsec Char T.Text m => m a -> m [a]
many1 p = do
  c <- p
  cs <- many p
  return (c : cs)

----
ident :: MonadParsec Char T.Text m => m T.Text
ident = (\h t -> T.pack (h:t))
        <$> printChar -- too weak?
        <*> many (alphaNumChar <|> char '_')

nonQuotedName :: MonadParsec Char T.Text m => m T.Text
nonQuotedName = T.pack <$> many1 letterChar

quotedName :: MonadParsec Char T.Text m => m T.Text
quotedName = T.pack <$> (char '"' >> manyTill L.charLiteral (char '"'))

name :: MonadParsec Char T.Text m => m T.Text
name = quotedName <|> nonQuotedName

--- Keyword name of elements of diagram
reserved :: MonadParsec Char T.Text m => T.Text -> m T.Text
reserved txt = do
  n <- lookAhead ident
  if n == txt then ident else empty


----
assocParser :: (MonadParsec Char T.Text m) => [(T.Text, m a)] -> m a
assocParser = choice . map pairParser

pairParser :: MonadParsec Char T.Text m => (T.Text, m a) -> m a
pairParser (txt, p) = lexeme (reserved txt) *> p


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


isEndWith endKey str = case res of
   Just _ -> True
   _ -> False
   where
     res = parseMaybe (spaceConsumer *> string (endMarker endKey) *> spaceConsumer) str

endMarker :: T.Text -> T.Text
endMarker endKey = T.append  "end " endKey

endOfLine :: MonadParsec Char T.Text m => m T.Text
endOfLine = crlf <|> (T.pack . (:[]) <$> newline)
