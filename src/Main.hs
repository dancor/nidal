module Main where

import Control.Applicative hiding (many, (<|>))
import Control.Monad
import Data.Maybe
import Text.Parsec hiding (parse)

data Grammar = Grammar [Either FeatureRule GrammarRule]
  deriving (Eq, Show)

data StringLiteral =
  StringLiteral String
  deriving (Eq, Show)

type GrammarRulePart = (String, [String])

data GrammarRule =
  GrammarRule GrammarRulePart [[Either StringLiteral GrammarRulePart]]
  deriving (Eq, Show)

data FeatureRule =
  FeatureRule String [String]
  deriving (Eq, Show)


class Parsable a where
  parse :: Parsec String () a

parseComment = string "//" >> many (satisfy (/= '\n')) <* char '\n'

parseWs = many $ many1 space <|> parseComment

instance Parsable StringLiteral where
  parse = char '"' >> StringLiteral <$> many (satisfy (/= '"')) <* char '"'

parseRuleWord = liftM2 (:) lower (many alphaNum)

parseFeature  = liftM2 (:) upper (many alphaNum)

parseRulePart :: Parsec String () (String, [String])
parseRulePart = liftM2 (,) parseRuleWord . fmap (fromMaybe []) . optionMaybe $
  char '(' >> sepBy (parseWs >> parseFeature <* parseWs) (char ',') <* char ')'

instance Parsable GrammarRule where
  parse = liftM2 GrammarRule parseRulePart $
    parseWs >> char '=' >> parseWs >>
    sepBy (many1 ((Left <$> parse <|> Right <$> parseRulePart) <* parseWs))
          (char '|' >> parseWs) <* char '.'

instance Parsable FeatureRule where
  parse = liftM2 FeatureRule parseFeature $
    parseWs >> char '=' >> parseWs >>
    sepBy (parseFeature <* parseWs) (char '|' >> parseWs) <* char '.'

instance Parsable Grammar where
  parse = Grammar <$> many1 ((Left <$> parse <|> Right <$> parse) <* parseWs)

printG :: Grammar -> IO ()
printG = print

main :: IO ()
main = do
  grammar <- readFile "grammar"
  case runParser (parse <* eof) () "grammar" grammar of
    Left err -> print err
    Right g -> printG g
