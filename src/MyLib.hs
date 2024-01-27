module MyLib where

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Char
import qualified Data.Foldable as F
import Data.List
import Prelude hiding (sequenceA)

replicate' :: Int -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n - 1) x

take' :: Int -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x : xs) = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' n (x : xs)
  | n == x = True
  | otherwise = n `elem'` xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let leq = [a | a <- xs, a <= x]
      gt = [a | a <- xs, a > x]
   in quicksort leq ++ [x] ++ quicksort gt

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' x xs = foldr (\y acc -> acc || (x == y)) False xs

maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 max

reverse'' :: [a] -> [a]
reverse'' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldl1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x acc -> if f x then x : acc else acc) []

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

encode :: Int -> String -> String
encode offset = map $ chr . (+ offset) . ord

decode :: Int -> String -> String
decode offset = encode $ negate offset

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstTo :: Int -> Maybe Int
firstTo n = find (\x -> digitSum x == n) [1 ..]

inputRPN :: String
inputRPN = "10 4 3 + 2 * -"

solveRPN :: String -> Double
solveRPN = head . foldl f [] . words
  where
    f (x : y : ys) "*" = (y * x) : ys
    f (x : y : ys) "+" = (y + x) : ys
    f (x : y : ys) "-" = (y - x) : ys
    f xs numberString = read numberString : xs

sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x : xs) = (:) <$> x <*> sequenceA xs

data Tree a
  = EmptyTree
  | Node a (Tree a) (Tree a)
  deriving (Show)

instance F.Foldable Tree where
  foldMap _ EmptyTree = mempty
  foldMap f (Node x l r) = F.foldMap f l <> f x <> F.foldMap f r

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
  (c', r') <-
    [ (c + 2, r - 1),
      (c + 2, r + 1),
      (c - 2, r - 1),
      (c - 2, r + 1),
      (c + 1, r - 2),
      (c + 1, r + 2),
      (c - 1, r - 2),
      (c - 1, r + 2)
      ]
  guard (c' `elem` [1 .. 8] && r' `elem` [1 .. 8])
  return (c', r')

in3 :: KnightPos -> [KnightPos]
in3 start = do
  first <- moveKnight start
  second <- moveKnight first
  moveKnight second

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start

newtype DiffList a = DiffList {getDiffList :: [a] -> [a]}

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Semigroup (DiffList a) where
  DiffList f <> DiffList g = DiffList (\xs -> f (g xs))

instance Monoid (DiffList a) where
  mempty = DiffList (\xs -> [] ++ xs)

gcd' :: Int -> Int -> Writer (DiffList String) Int
gcd' a b
  | b == 0 = do
      tell (toDiffList ["Finished with " ++ show a])
      return a
  | otherwise = do
      result <- gcd' b (a `mod` b)
      tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
      return result

addStuff :: Int -> Int
addStuff = do
  a <- (* 2)
  b <- (+ 10)
  return (a + b)

keepSmall :: Int -> Writer [String] Bool
keepSmall x
  | x < 4 = do
      tell ["Keeping " ++ show x]
      return True
  | otherwise = do
      tell [show x ++ " is too large, throwing it away"]
      return False

powerset :: [a] -> [[a]]
powerset = filterM (\_ -> [True, False])

binSmalls :: Int -> Int -> Maybe Int
binSmalls acc x
  | x > 9 = Nothing
  | otherwise = Just (acc + x)

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of
  [(x, "")] -> Just x
  _ -> Nothing

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x : y : ys) "*" = return ((y * x) : ys) -- Multiplies the top two numbers.
foldingFunction (x : y : ys) "+" = return ((y + x) : ys) -- Adds the top two numbers.
foldingFunction (x : y : ys) "-" = return ((y - x) : ys) -- Subtracts the top two numbers.
foldingFunction xs numberString = liftM (: xs) (readMaybe numberString)

solveRPN :: String -> Maybe Double
solveRPN st = do
  [result] <- foldM foldingFunction [] (words st)
  return result
