module Homework1 where

import Data.Char

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
