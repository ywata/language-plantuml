{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.PlantUML.ParserHelper where
import Text.Megaparsec hiding(parse, parseMaybe)
import qualified Text.Megaparsec as M
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T

--parse :: Parsec Char T.Text a -> String -> T.Text -> Either (ParseErrorBundle T.Text Char) a
parse :: Parsec Char T.Text a -> String -> T.Text -> Either (ParseErrorBundle T.Text Char) a
parse p txt = M.parse p txt . T.pack . dropContinuationLine . T.unpack

parseMaybe :: Parsec Char T.Text a -> T.Text -> Maybe a
parseMaybe p = M.parseMaybe p . T.pack . dropContinuationLine . T.unpack

-- slow
dropContinuationLine :: String -> String
dropContinuationLine = go
  where
    go :: String -> String
    go [] = []
    go ('\\' : '\\' :  xs) = '\\' : go xs
    go ('\\' : '\n' : xs) = go xs
    go ('\\' : '\r' : '\n' : xs) = go xs
    go (x : xs) = x : go xs

spaceConsumer :: MonadParsec Char T.Text m => m ()
spaceConsumer = L.space space1 (L.skipLineComment "'") (L.skipBlockComment "/'" "'/")
spaceConsumer' :: MonadParsec Char T.Text m => m ()
spaceConsumer' = L.space (spaceChar >> pure()) (L.skipLineComment "'") (L.skipBlockComment "/'" "'/")

lexeme :: MonadParsec Char T.Text m => m a -> m a
lexeme  = L.lexeme spaceConsumer

lexeme':: MonadParsec Char T.Text m => m a -> m a
lexeme'  = L.lexeme spaceConsumer'


symbol :: MonadParsec Char T.Text m => T.Text -> m T.Text
symbol  = L.symbol spaceConsumer
symbol' :: MonadParsec Char T.Text m => T.Text -> m T.Text
symbol'  = L.symbol spaceConsumer'

mkAssoc :: (Show a, Enum a, Bounded a, MonadParsec Char T.Text m) => [(T.Text, m a)]
mkAssoc = map (\e -> (T.toLower . T.pack . show $ e, return e)) $ enumFromTo minBound maxBound


----
many1:: MonadParsec Char T.Text m => m a -> m [a]
many1 p = do
  c <- p
  cs <- many p
  return (c : cs)

----
ident' :: MonadParsec Char T.Text m => m T.Text
ident' = (\h t -> T.pack (h:t))
        <$> (letterChar <|> char '@') -- should be alphabet only
        <*> (many (alphaNumChar <|> char '_'))

ident :: MonadParsec Char T.Text m => m T.Text
ident = lexeme ident'


nonQuotedName :: MonadParsec Char T.Text m => m T.Text
nonQuotedName = T.pack <$> lexeme (many1 (letterChar <|> digitChar))

quotedName :: MonadParsec Char T.Text m => m T.Text
quotedName = T.pack <$> lexeme ((char '"' >> manyTill printChar (char '"')))


--- Keyword name of elements of diagram
--- non lexeme version of reserved
reserved' :: MonadParsec Char T.Text m => T.Text -> m T.Text
reserved' txt = do
  n <- lookAhead ident'
  if T.toCaseFold n == T.toCaseFold txt then ident' else empty
reserved :: MonadParsec Char T.Text m => T.Text -> m T.Text
reserved txt = lexeme (reserved' txt)



{-- TODO: This may need update. -}
reservedSymbol :: MonadParsec Char T.Text m => T.Text -> m T.Text
reservedSymbol txt = do
  n <- lookAhead (string txt <* (choice [space, endOfLine >> pure ()]))
  if T.toCaseFold n == T.toCaseFold txt then (lexeme (string txt)) else empty
  

----
assocParser :: (MonadParsec Char T.Text m) => [(T.Text, m a)] -> m a
assocParser = lexeme . choice . map pairParser

pairParser :: MonadParsec Char T.Text m => (T.Text, m a) -> m a
pairParser (txt, p) = lexeme (reserved txt) *> p

----
oneLine :: MonadParsec Char T.Text m => m [T.Text]
oneLine = ((:[]) . T.pack) <$> manyTill printChar endOfLine

---- restOfLine treates continuation line or virtual one line

restOfLine :: MonadParsec Char T.Text m => m T.Text
restOfLine = T.pack <$> manyTill printChar endOfLine
{- where
    restOfLine' :: MonadParsec Char T.Text m => [T.Text] -> m [T.Text]
    restOfLine' xs = do
      l <- T.pack <$> manyTill printChar endOfLine
      if isContinueLine l then
        restOfLine' (l:xs)
        else
        return $ reverse (l : xs)
isContinueLine :: T.Text -> Bool
isContinueLine = T.isSuffixOf "\\"
-}
{-
multiLine :: MonadParsec Char T.Text m => (T.Text -> Bool) -> m [T.Text]
multiLine = multiLine' []

multiLine' :: MonadParsec Char T.Text m => [T.Text] -> (T.Text ->Bool) -> m [T.Text]
multiLine' xs prep = do
  l <- T.pack <$> manyTill printChar endOfLine
  if prep l then
    return (reverse xs)
  else
    multiLine' (l:xs) prep
-}


--miscParsers :: MonadParsec Char T.Text m => [(String, m Command)]
--miscParsers = [(Autonumber, triple (optional Nothing(lexeme L.

{-
isEndWith endKey str = case res of
   Just _ -> True
   _ -> False
   where
     res :: MonadParsec Char T.Text m => m ()
     res = parseMaybe (spaceConsumer *> string (endMarker endKey) *> spaceConsumer) str
-}
endMarker :: T.Text -> T.Text
endMarker endKey = T.append  "end " endKey

endOfLine :: MonadParsec Char T.Text m => m ()
endOfLine = (crlf >> pure ()) <|> (newline >> pure())

