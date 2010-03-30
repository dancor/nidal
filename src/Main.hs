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

data GrammarRulePart = GrammarRulePart {
  grammarRulePartName :: String,
  grammarRulePartFeatures :: [String]
  }
  deriving (Eq, Show)

data GrammarRule = GrammarRule {
  grammarRuleLHS :: GrammarRulePart,
  grammarRuleRHS :: [[Either StringLiteral GrammarRulePart]]
  }
  deriving (Eq, Show)

class Parsable a where
  parse :: Parsec String () a

parseComment = string "//" >> many (satisfy (/= '\n')) <* char '\n'

parseWs = many $ many1 space <|> parseComment

instance Parsable StringLiteral where
  parse = char '"' >> StringLiteral <$> many (satisfy (/= '"')) <* char '"'

parseRuleWord = liftM2 (:) lower (many alphaNum)

parseFeature  = liftM2 (:) upper (many alphaNum)

instance Parsable GrammarRulePart where
  parse = liftM2 GrammarRulePart parseRuleWord . fmap (fromMaybe []) .
    optionMaybe $ char '(' >>
    sepBy (parseWs >> parseFeature <* parseWs) (char ',') <* char ')'

instance Parsable GrammarRule where
  parse = liftM2 GrammarRule parse $
    parseWs >> char '=' >> parseWs >>
    sepBy (many1 ((Left <$> parse <|> Right <$> parse) <* parseWs))
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

featureParent :: String -> (M.Map String [String]) -> String
featureParent feature featureMap = if M.member feature featureMap
  then feature
  else fst . head . filter ((feature `elem`) . snd) $ M.toList featureMap

{-
randProduction :: String -> [M.Map String (S.Set String)] -> Grammar ->
  Rand StdGen [String]
randProduction ruleName features (Grammar gFeatures gRules) =
  --return $ map show rulesMatchingRule
  return $ map show ruleFeatures
  where
  rulesMatchingRule = filter
    ((== ruleName) . grammarRulePartName . grammarRuleLHS) gRules
  ruleFeatures = map (flip featureParent gFeatures) . grammarRulePartFeatures .
    grammarRuleLHS $ head rulesMatchingRule
  --rulesMatchingFeat = filter ((== rule) . fst . grammarRuleLHS) rulesMatchingRule
-}

-- this will get interesting with an infinite-size-language grammar
allProductions :: String -> [M.Map String (S.Set String)] -> Grammar ->
  [[String]]
allProductions ruleName featureConstraints (Grammar gFeatureMap gRules) =
  where
  rulesMatchingRule = filter
    ((== ruleName) . grammarRulePartName . grammarRuleLHS) gRules
  ruleFeatures = map (flip featureParent gFeatures) . grammarRulePartFeatures .
    grammarRuleLHS $ head rulesMatchingRule

main :: IO ()
main = do
  grammarContent <- readFile "grammar"
  case runParser (parse <* eof) () "grammar" grammarContent of
    Left err -> print err
    Right grammar -> do
      --sentence <- evalRandIO $ randProduction "sentence" [] grammar
      sentence <- evalRandIO $ randProduction "pronoun" [] grammar
      --print sentence
      mapM_ print sentence
