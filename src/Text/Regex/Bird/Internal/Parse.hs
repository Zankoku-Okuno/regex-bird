module Text.Regex.Bird.Internal.Parse where

import Data.Char
import Data.Symbol
import Data.Text (Text)
import qualified Data.Text as T
import Text.Regex.Bird.Patterns
import Text.Regex.Bird.Text
import Control.Applicative
import Data.Attoparsec.Text


parseRegex :: Text -> Either String Regex
parseRegex = parseOnly (regex <* endOfInput)

regex :: Parser Regex
regex = altRegex

altRegex :: Parser Regex
altRegex = do
    products <- andRegex `sepBy1` (char '|')
    pure $ foldr1 Alt products

andRegex :: Parser Regex
andRegex = do
    terms <- seqRegex `sepBy1` (char '&')
    pure $ foldr1 And terms

seqRegex :: Parser Regex
seqRegex = do
    parts <- many atomicRegex
    pure $ foldr Seq Empty parts

atomicRegex :: Parser Regex
atomicRegex = do
    prefixes <- many prefix
    base <- choice
                [ top
                , parenthesized
                -- character classes
                , escaped
                , literal
                ]
    suffixes <- many suffix
    let applyPrefixes = foldr (.) id prefixes
        applySuffixes = foldl (.) id suffixes
    pure $ (applySuffixes . applyPrefixes) base


top :: Parser Regex
top = do
    char '.'
    pure Any

escaped :: Parser Regex
escaped = do
    char '\\'
    Char <$> anyChar

literal :: Parser Regex
literal = do
    Char <$> satisfy (notInClass specialChars)


parenthesized :: Parser Regex
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
        char '.'
        pure $ ctor <$> regex
    replay = do
        char '='
        ctor <- Replay <$> symbol
        pure $ pure ctor

prefix :: Parser (Regex -> Regex)
prefix = do
    char '^'
    pure Not

suffix :: Parser (Regex -> Regex)
suffix = choice
    [ Star <$ char '*'
    , Plus <$ char '+'
    , Option <$ char '?'
    -- TODO {n,m}
    ]


symbol :: Parser Symbol
symbol = intern . T.unpack <$> takeWhile1 isAlphaNum

specialChars = ".\\&|^?*+()[]{}"
