{-# LANGUAGE LambdaCase #-}

module Scryptic.Parser (
  parseScript,
) where

import Scryptic.RuntimeOptions
import Scryptic.Scrypt

import Control.Applicative ( Applicative(..), (<$>), (<$), (*>), liftA2)
import Text.Parsec
import Text.Parsec.Text
import Data.Char
import Data.Text (Text)

parseScript :: Text -> Either ParseError Scrypt
parseScript inp = runP parseAll () "input" inp
  where
    lineParsers =
        [ parseTrigger
        , parseWait
        , parseWrite
        , parseWatch
        , parseUnwatch
        , parseSleep
        , parseOpt ]
    parseAll = spaces
               *> many (choice lineParsers)
               <* eof

parseTrigger :: Parser ScryptStatement
parseTrigger = do
    key <- parseReservedKey "trigger" <* spaces
    optionMaybe (parseReservedKey "sync" <* spaces) >>= \case
        Just sKey -> return $ TriggerSync key sKey
        Nothing -> return $ Trigger key

-- parse a 'write' line
parseWrite :: Parser ScryptStatement
parseWrite =
    Write <$> parseReservedKey "write"
          <* spaces <*> (parseString <|> parseNum) <* spaces

parseWait :: Parser ScryptStatement
parseWait = Wait <$> parseReservedKey "wait" <* spaces

parseWatch :: Parser ScryptStatement
parseWatch = Watch <$> parseReservedKey "watch" <* spaces

parseUnwatch :: Parser ScryptStatement
parseUnwatch = Unwatch <$> parseReservedKey "unwatch" <* spaces

parseSleep :: Parser ScryptStatement
parseSleep = do
    str <- string "sleep" *> spaces *> parseNum
    case read str of
        Just n -> Sleep n <$ spaces
        Nothing -> fail $ concat
            [ "scryptic: sleep: doesn't look like a number, `"
            , str, "'" ]

parseOpt :: Parser ScryptStatement
parseOpt = do
    optName <- parseReservedKey "opt" <* spaces
    optVal <- many (satisfy (not . isSpace))
    let msgStr = concat ["key <",optName,"> val <",optVal,">"]
    flip SetOpt msgStr <$> getValuedOptionSetter optName optVal <* spaces

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
