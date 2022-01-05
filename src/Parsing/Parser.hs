{-# LANGUAGE TupleSections #-}

module Parsing.Parser where

import Control.Applicative (Alternative (empty, (<|>)), liftA2)
import Control.Monad (MonadPlus)
import Data.Bifunctor (first)
import Data.Functor ((<&>))

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

parse :: Parser a -> String -> Maybe a
parse p = fmap fst . runParser p

instance Functor Parser where
  fmap f pa = Parser $ fmap (first f) . runParser pa

instance Applicative Parser where
  pure a = Parser $ pure . (a,)
  pf <*> pa = Parser $ \s -> do
    (f, s') <- runParser pf s
    runParser pa s' <&> first f

instance Alternative Parser where
  empty = Parser $ const Nothing
  Parser p1 <|> Parser p2 = Parser $ liftA2 (<|>) p1 p2

instance Monad Parser where
  pa >>= f = Parser $ \s -> do
    (a, s') <- runParser pa s
    runParser (f a) s'

instance MonadPlus Parser

instance Semigroup a => Semigroup (Parser a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (Parser a) where
  mempty = pure mempty
