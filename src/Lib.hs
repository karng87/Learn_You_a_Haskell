module Lib where

import           Control.Monad.Writer
import           Data.Monoid


--import qualified Data.ByteString as B
--import Data.Monoid
--import qualified Control.Monad as CM
--import qualified Control.Monad.Trans.Writer.Lazy as W

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _       = ("beer", Sum 30)

applyLog ::(Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f = let (x', newLog) = f x
                      in (x', log `mappend` newLog)


logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
          a <- logNumber 3
          b <- logNumber 5
          return (a * b)

--gcd' :: Int -> Int -> Int
--gcd' x y
--      | y == 0 = x
--      | otherwise = gcd' y (x `mod` y)
--
gcd'' :: Int -> Int -> Writer [String] Int
gcd'' x y
      | y == 0 = do
                tell ["Finished with " ++ show x]
                return x
      | otherwise = do
                tell [show x ++ " mod " ++ show y ++ " = " ++ show (x `mod` y)]
                gcd'' y (x `mod` y)

gcdReverse :: Int -> Int -> Writer [String] Int
gcdReverse x y
      | y == 0 = do
                tell ["Finished with " ++ show x]
                return x
      | otherwise = do
                result <- gcdReverse y (x `mod` y)
                tell [show x ++ " mod " ++ show y ++ " = " ++ show (x `mod` y)]
                return result

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }
toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++)
fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Semigroup (DiffList a) where
  (DiffList f) <> (DiffList g) = DiffList (\xs -> f . g $ xs)

instance Monoid (DiffList a) where
  mempty  = DiffList (\xs -> [] ++ xs)
  mappend = (<>)

gcd' :: Int -> Int -> Writer (DiffList String) Int
gcd' x y
      | y == 0 = do
                tell (toDiffList ["Finished with " ++ show x])
                return x
      | otherwise = do
                result <- gcd' y (x `mod` y)
                tell (toDiffList [show x ++ " mod " ++ show y ++ " = " ++ show (x `mod` y)])
                return result

finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = do
                tell (toDiffList ["0"])
finalCountDown x = do
                finalCountDown (x -1)
                tell (toDiffList [show x])

finalCountDown' :: Int -> Writer [String] ()
finalCountDown' 0 = do
                    tell ["0"]
finalCountDown' x = do
                    finalCountDown' (x-1)
                    tell [show x]




