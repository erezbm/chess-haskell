module Parsing.Combinator where

import Control.Applicative (Alternative (empty))
import Control.Monad (mfilter)
import Data.Char (digitToInt, isDigit, isLetter, isSpace)
import Data.Foldable (asum)
import Data.Functor ((<&>))
import Data.List (uncons)
import Parsing.Parser (Parser (Parser))

anyChar :: Parser Char
anyChar = Parser uncons

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = mfilter f anyChar

char :: Char -> Parser Char
char = satisfy . (==)

anyDigit :: Parser Int
anyDigit = satisfy isDigit <&> digitToInt

anyLetter :: Parser Char
anyLetter = satisfy isLetter

space :: Parser Char
space = satisfy isSpace

choice :: (Foldable t) => t (Parser a) -> Parser a
choice = asum

eof :: Parser ()
eof = Parser $ \s -> if null s then pure ((), s) else empty
