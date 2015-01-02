module Homework1 where

import Data.Char

-- Validating Credit Card Numbers

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = map (toInteger . digitToInt) $ show n

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse $ toDigits n

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:(y:xs)) = x : y * 2 : doubleEveryOther xs

sumDigits :: [Integer] -> Integer
sumDigits = foldr ((+) . sum . toDigits) 0

validate :: Integer -> Bool
validate n = sumDigits(doubleEveryOther $ toDigitsRev n) `mod` 10 == 0

-- The Towers of Hanoi

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 1 a b _ _ = [(a, b)]
hanoi4 2 a b c _ = [(a, c), (a, b), (c, b)]
hanoi4 n a b c d = hanoi4 (n - 2) a c d b ++ hanoi4 2 a b d c ++ hanoi4 (n - 2) c b a d
