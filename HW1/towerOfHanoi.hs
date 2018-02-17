module Hanoi where

type Peg = String
type Move = (Peg, Peg)


-- Return list of moves to move n discs from the first Peg to the second.
-- hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
-- 1. move n-1 discs from a to c using b as temporary storage
-- 2. move the top disc from a to b
-- 3. move n-1 discs from c to b using a as temporary storage
hanoi3 :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi3 n a b c
  | n == 0 = []
  | otherwise = step1 ++ step2 ++ step3
    where
      step1 = hanoi3 (n-1) a c b
      step2 = [(a, b)]
      step3 = hanoi3 (n-1) c b a


hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n a b c d
  | n == 0 = []
  | n == 1    = [(a, b)]
  | n == 2    = [(a, c), (a, b), (c, b)]
  | n == 3    = [(a, c), (a, d), (a, b), (d, b), (c, b)]
  | otherwise = step1 ++ step2 ++ step3
    where
      step1 = hanoi4 (n-1) a c b d
      step2 = hanoi4 1 a b c d
      step3 = hanoi4 (n-1) c b a d
