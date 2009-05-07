module Main where

import Control.Applicative hiding (many, (<|>))
import Control.Arrow
import Control.Monad
import Control.Monad.Random
import Data.Either
import Data.Maybe
import FUtil hiding (satisfy)
import Text.Parsec hiding (parse)
import qualified Data.Map as M
import qualified Data.Set as S

data Grammar = Grammar (M.Map String [String]) [GrammarRule]
  deriving (Eq, Show)

data StringLiteral =
  StringLiteral String
  deriving (Eq, Show)

type GrammarRulePart = (String, [String])

data GrammarRule =
  GrammarRule GrammarRulePart [[Either StringLiteral GrammarRulePart]]
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

parseFeatureRule = liftM2 (,) parseFeature $
  parseWs >> char '=' >> parseWs >>
  sepBy (parseFeature <* parseWs) (char '|' >> parseWs) <* char '.'

instance Parsable Grammar where
  parse = fmap (uncurry Grammar . first M.fromList . partitionEithers) $
    parseWs >>
    many1 ((Left <$> parseFeatureRule <|> Right <$> parse) <* parseWs)

printG :: Grammar -> IO ()
printG = print

randProduction :: String -> [M.Map String (S.Set String)] -> Grammar ->
  Rand StdGen [String]
randProduction rule features grammar = undefined

main :: IO ()
main = do
  grammarContent <- readFile "grammar"
  case runParser (parse <* eof) () "grammar" grammarContent of
    Left err -> print err
    Right grammar -> do
      sentence <- evalRandIO $ randProduction "sentence" [] grammar
      print sentence
      --printG grammar
