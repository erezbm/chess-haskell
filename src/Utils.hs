module Utils where

import Control.Applicative (Alternative (empty))
import Data.Ix (Ix (range))
import Data.List (unfoldr)
import Data.Tuple (swap)

generateList :: (a -> Maybe a) -> a -> [a]
generateList f = unfoldr (fmap (\a -> (a, a)) . f)

filterMaybe :: (a -> Bool) -> a -> Maybe a
filterMaybe p a
  | p a = Just a
  | otherwise = Nothing

fmapMaybe :: (Monad m, Alternative m) => (a -> Maybe b) -> m a -> m b
fmapMaybe f pa = pa >>= maybe empty return . f

boolToMaybe :: Bool -> a -> Maybe a
boolToMaybe = filterMaybe . const

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast l = Just (last l)

rangeAbs :: Ix a => (a, a) -> [a]
rangeAbs r@(from, to)
  | from <= to = range r
  | otherwise = range (swap r)
