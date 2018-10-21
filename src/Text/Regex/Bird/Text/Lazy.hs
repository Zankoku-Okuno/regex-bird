module Text.Regex.Bird.Text.Lazy where

import Data.Symbol
import Data.Text.Lazy (Text)
import Text.Regex.Bird


type Regex = GRegex Symbol Text Char
