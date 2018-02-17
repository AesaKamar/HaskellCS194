module CreditCards  where

import           Data.Foldable


-- Exercise 1 We need to first find the digits of a number. Define the
-- functions
-- toDigits :: Integer -> [Integer]
-- toDigitsRev :: Integer -> [Integer]
-- toDigits should convert positive Integers to a list of digits. (For 0 or
-- negative inputs, toDigits should return the empty list.) toDigitsRev
-- should do the same, but with the digits reversed.
-- Example: toDigits 1234 == [1,2,3,4]
-- Example: toDigitsRev 1234 == [4,3,2,1]
-- Example: toDigits 0 == []
-- Example: toDigits (-17) == []

toDigitsRev :: Integer -> [Integer]
toDigitsRev n =
  if n < 1
    then []
    else remainder : toDigitsRev quotient
  where
    (quotient, remainder) = n `divMod` 10

toDigits :: Integer -> [Integer]
toDigits  = reverse . toDigitsRev


-- Exercise 2 Once we have the digits in the proper order, we need to
-- double every other one. Define a function
-- doubleEveryOther :: [Integer] -> [Integer]
-- Remember that doubleEveryOther should double every other number
-- beginning from the right, that is, the second-to-last, fourth-to-last,
-- . . . numbers are doubled.
-- Example: doubleEveryOther [8,7,6,5] == [16,7,12,5]
-- Example: doubleEveryOther [1,2,3] == [1,4,3]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther ints =
  case ints of
    [x1, x2, x3] -> [x1, x2*2, x3]
    x1: x2: xs   -> x1*2 : x2 : doubleEveryOther xs
    xs           -> xs


-- Exercise 3 The output of doubleEveryOther has a mix of one-digit
-- and two-digit numbers. Define the function
-- sumDigits :: [Integer] -> Integer
-- to calculate the sum of all digits.
-- Example: sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
sumDigits :: [Integer] -> Integer
sumDigits = fold

instance Monoid Integer where
  mempty = 0
  mappend a1 a2 = sum (toDigits a1)  + a2


-- Exercise 4 Define the function
-- validate :: Integer -> Bool
-- that indicates whether an Integer could be a valid credit card number.
-- This will use all functions defined in the previous exercises.
-- Example: validate 4012888888881881 = True
-- Example: validate 4012888888881882 = False
validate :: Integer -> Bool
validate n = (sumDigits( doubleEveryOther( toDigits n)) `mod` 10) == 0










    -- End
