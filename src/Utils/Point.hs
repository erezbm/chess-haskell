{-# LANGUAGE DeriveFunctor #-}

module Utils.Point where

import Control.Applicative (liftA2)

data Point a = Point a a deriving (Functor, Eq)

instance Applicative Point where
  pure x = Point x x
  (Point fx fy) <*> (Point x y) = Point (fx x) (fy y)

instance Num a => Num (Point a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger
  negate = fmap negate
