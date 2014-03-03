{-# LANGUAGE LambdaCase #-}

module Scryptic.Parser (
  parseScript,
  parseFile,
) where

import Scryptic.RuntimeOptions
import Scryptic.Scrypt

import Control.Applicative ( Applicative(..), (<$>), (<$), (*>), liftA2)
import Text.Parsec
import Text.Parsec.Text
import Data.Char
import Data.Text (Text)
import qualified Data.Text.IO as Text

parseScript :: Text -> Either ParseError Scrypt
parseScript inp = runP parseAll () "input" inp
  where
    parseAll = spacesComments *> many parseBlock <* eof

parseFile :: FilePath -> IO (Either ParseError Scrypt)
parseFile file = parseScript <$> Text.readFile file

-- this never returns an empty block, which means it can be called with
-- `many` itself.
parseBlock :: Parser ScryptBlock
parseBlock =
    mkBlock <$> optionMaybe (parseReservedKey "title" <* spacesComments)
            <*> many1 statementParsers
  where
    mkBlock Nothing xs = ScryptBlock xs
    mkBlock (Just title) xs = TitledBlock title xs

statementParsers :: Parser ScryptStatement
statementParsers = choice
        [ parseTrigger
        , parseWait
        , parseWrite
        , parseWatch
        , parseUnwatch
        , parseSleep
        , parseOpt ]

parseTrigger :: Parser ScryptStatement
parseTrigger = do
    key <- parseReservedKey "trigger" <* spacesComments
    optionMaybe (parseReservedKey "sync" <* spacesComments) >>= \case
        Just sKey -> return $ TriggerSync key sKey
        Nothing -> return $ Trigger key

-- parse a 'write' line
parseWrite :: Parser ScryptStatement
parseWrite =
    Write <$> parseReservedKey "write"
          <* spaces <*> (parseString <|> parseNum) <* spacesComments

parseWait :: Parser ScryptStatement
parseWait = Wait <$> parseReservedKey "wait" <* spacesComments

parseWatch :: Parser ScryptStatement
parseWatch = Watch <$> parseReservedKey "watch" <* spacesComments

parseUnwatch :: Parser ScryptStatement
parseUnwatch = Unwatch <$> parseReservedKey "unwatch" <* spacesComments

parseSleep :: Parser ScryptStatement
parseSleep = do
    str <- string "sleep" *> spaces *> parseNum
    case read str of
        Just n -> Sleep n <$ spacesComments
        Nothing -> fail $ concat
            [ "scryptic: sleep: doesn't look like a number, `"
            , str, "'" ]

parseOpt :: Parser ScryptStatement
parseOpt = do
    optName <- parseReservedKey "opt" <* spacesComments
    optVal <- many (satisfy (not . isSpace))
    let msgStr = concat ["set <",optName,">  <",optVal,">"]
    flip SetOpt msgStr <$> getValuedOptionSetter optName optVal
                       <* spacesComments

parseReservedKey :: String -> Parser String
parseReservedKey reserved =
    try (string reserved) *> spaces *> many (satisfy (not . isSpace))

-- Parse a (single-line) string literal
parseString :: Parser String
parseString = concat <$> between (char '"') (char '"' <?> "end of string")
    (many stringChars)
  where
    stringChars = ((\a b -> [a,b]) <$> char '\\' <*> anyChar)
                 <|> ((:[]) <$> noneOf ['"'])

-- regular decimal-style numbers, optional floating point, optional
-- scientific notation
parseNum :: Parser String
parseNum = (\a b c d -> concat [a,b,c,d])
    <$> option "" (string "-")
    <*> many1 digit
    <*> option "" (mPrefix '.' (many1 digit))
    <*> option "" (liftA2 (++) (string "e")
        (liftA2 (++) (option "" (string "-")) (many1 digit)) )
  where
    mPrefix c p = liftA2 (++) (string [c]) p

-- before long I'll want a tokenizing parser.  When it gets to that point,
-- I might prefer an actual grammar though.
parseComment :: Parser ()
parseComment = () <$ string "--" <* skipMany (noneOf "\n") <* char '\n'

spacesComments :: Parser ()
spacesComments = spaces *> sp'
  where
    sp'  = skipMany (parseComment *> spaces)
