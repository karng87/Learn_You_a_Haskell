{-# LANGUAGE FlexibleContexts #-}
module FunctorApplicative where

import           Control.Applicative
import qualified Data.Foldable       as F
import           Data.Monoid

newtype Pair b a = Pair  {getPair :: (a, b)}

instance Functor (Pair c) where
  --fmap :: (a -> b) -> Pair c a -> Pair c b
  fmap f (Pair (x, y)) = Pair (f x, y)

-- lengthCompare :: String -> String -> Ordering
-- lengthCompare x y = let a = length x `compare` length y
--                         b = x `compare` length y
--                     in if a == EQ then b else a

lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend` (x `compare` y)


newtype First' a = First' {getFirst' :: Maybe a}
  deriving (Eq, Ord, Read, Show)

instance Semigroup (First' a) where
  (<>) = mappend

instance Monoid (First' a) where
  mempty = First' Nothing
  First' (Just x) `mappend` _ = First' (Just x)
  First' Nothing `mappend` x = x

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

instance F.Foldable Tree where
  foldMap f Empty        = mempty
  foldMap f (Node x l r) = F.foldMap f l `mappend` f x `mappend` F.foldMap f r

testTree = Node 5
            (Node 3
                (Node 1 Empty Empty)
                (Node 6 Empty Empty)
            )
            (Node 9
                (Node 8 Empty Empty)
                (Node 10 Empty Empty)
            )

type Birds = Int
type Pole = (Birds,Birds)

landLeft' :: Birds -> Pole -> Pole
landLeft' x (l,r) = (x+l,r)

landRight' :: Birds -> Pole -> Pole
landRight' x (l,r) = (l,x+r)

x -: f = f x

landLeft :: Birds -> Pole -> Maybe Pole
landLeft x (l,r)
                | abs ( l + x - r ) < 4     = Just (l + x, r)
                | otherwise                 = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight x (l,r) 
                | abs ( l - ( x + r)) < 4   = Just (l, x + r)
                | otherwise                 = Nothing



