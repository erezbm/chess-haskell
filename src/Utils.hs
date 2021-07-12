module Utils where

import Data.List

generateList :: (a -> Maybe a) -> a -> [a]
generateList f = unfoldr (fmap (\a -> (a, a)) . f)

filterMaybe :: (a -> Bool) -> a -> Maybe a
filterMaybe p a
  | p a = Just a
  | otherwise = Nothing

boolToMaybe :: Bool -> a -> Maybe a
boolToMaybe = filterMaybe . const
