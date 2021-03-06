module Text.Regex.Bird.Internal.Parse where

import Data.Char
import qualified Data.String
import Data.ListLike (ListLike, StringLike(fromString))
import Data.RangeSet.Map (RSet)
import qualified Data.RangeSet.Map as R

import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Text as T

import Text.Regex.Bird.Patterns


type CharRegex x s = (StringLike s, ListLike s Char, StringLike x, Ord s, Ord x)


parseRegex :: (CharRegex x s) => String -> Either String (GRegex x s Char)
parseRegex = parseOnly (regex <* endOfInput) . T.pack

instance (CharRegex x s) => Data.String.IsString (GRegex x s Char) where
    fromString str = case parseRegex str of
        Left err -> error $ "Regex parse error: " ++ err
        Right r -> r


regex :: CharRegex x s => Parser (GRegex x s Char)
regex = altRegex

altRegex :: CharRegex x s => Parser (GRegex x s Char)
altRegex = do
    products <- andRegex `sepBy1` (char '|')
    pure $ foldr1 Alt products

andRegex :: CharRegex x s => Parser (GRegex x s Char)
andRegex = do
    terms <- seqRegex `sepBy1` (char '&')
    pure $ foldr1 And terms

seqRegex :: CharRegex x s => Parser (GRegex x s Char)
seqRegex = do
    parts <- many affixRegex
    pure $ foldr Seq Empty parts

affixRegex :: CharRegex x s => Parser (GRegex x s Char)
affixRegex = do
    prefixes <- many prefix
    base <- atomRegex
    suffixes <- many suffix
    let applyPrefixes = foldr (.) id prefixes
        applySuffixes = foldl (.) id suffixes
    pure $ (applySuffixes . applyPrefixes) base

atomRegex :: CharRegex x s => Parser (GRegex x s Char)
atomRegex = choice
    [ top
    , parenthesized
    , charClass
    , escaped
    , literal
    ]


top :: CharRegex x s => Parser (GRegex x s Char)
top = do
    char '.'
    pure Any

escaped :: CharRegex x s => Parser (GRegex x s Char)
escaped = do
    char '\\'
    Char <$> anyChar

charClass :: CharRegex x s => Parser (GRegex x s Char)
charClass = do
    char '['
    ctor <- option Elem (NotElem <$ char '^')
    cs <- many classEntry
    char ']'
    pure $ ctor cs

literal :: CharRegex x s => Parser (GRegex x s Char)
literal = do
    Char <$> satisfy (notInClass specialChars)

parenthesized :: CharRegex x s => Parser (GRegex x s Char)
parenthesized = do
    char '('
    k <- choice
        [ capture
        , replay
        , pure regex
        ]
    it <- k
    char ')'
    pure it
    where
    capture = do
        char '?'
        ctor <- Capture <$> symbol
        char '='
        pure $ ctor <$> regex
    replay = do
        char '='
        ctor <- Replay <$> symbol
        pure $ pure ctor


prefix :: CharRegex x s => Parser (GRegex x s Char -> GRegex x s Char)
prefix = do
    char '^'
    pure Not

suffix :: CharRegex x s => Parser (GRegex x s Char -> GRegex x s Char)
suffix = choice
    [ Star <$ char '*'
    , Plus <$ char '+'
    , Option <$ char '?'
    , boundedRep
    ]

boundedRep :: CharRegex x s => Parser (GRegex x s Char -> GRegex x s Char)
boundedRep = do
    char '{'
    low <- option 0 number
    char ','
    high <- optional number
    char '}'
    pure $ Rep (low, high)
    where
    number :: Parser Int
    number = read <$> many1 digit


symbol :: StringLike x => Parser x
symbol = fromString . T.unpack <$> takeWhile1 isAlphaNum

classEntry :: Parser (Char, Char)
classEntry = choice [range, single]
    where
    range = do
        low <- aChar
        char '-'
        high <- aChar
        pure (low, high)
    single = do
        c <- aChar
        pure (c, c)
    aChar = choice
        [ char '\\' *> anyChar
        , satisfy (\c -> c /= '\\' && c /= ']')
        ]

specialChars = ".\\&|^?*+()[]{}"
