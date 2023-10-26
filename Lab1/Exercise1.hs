module Exercise1 where

import Data.List
import System.Random
import Test.QuickCheck

{-

---- EXERCISE 1 ---------------------------------------------------------------

    > Time Spent: 1h

    > Description:

        We are asked to write a Haskell function that finds the factorial of
    an Integer input with the following specification:

        factorial :: Integer -> Integer

        The function should be written utilising recursion. To do that, we 
    could actually write something like this:

        factorial :: Integer -> Integer
        factorial n | n == 0 = 1
                    | otherwise  = n * factorial(n-1)

        However, this implementation doesn't make use of tail recursion, a
    powerful Haskell feature to avoid stack calls. For our implementation
    we decided to make use of tail recursion.

    > Testing:

        To test our functions we are asked to submit at least two props. Here we 
    include a description of some of the considered tests
     - Prop1    (n+1)! = n! * (n+1)
     - Prop2    n! mod k == 0, k in {1..n}
     - Prop3    (n/2)^n > n! > (n/3)^3, n > 5

        We have created custom generators for each of the propositions and also
    have written them using preconditions as a way to practice our Haskell skills.
    However, it is always better to use a generator, as we have to test our
    functions withing their domain. For this reason, we left commented the 
    preconditions propositions and left uncommented the generator prepared ones.

        We would like to note that for prop_2 we created gen2, which uses 
    every n >= 1. The condition k==0 is just used as the base case, but we
    never consider division by 0, as 0 does not belong to the domain.

-}

-- > Function       -----------------------------------------------------------
factorial :: Integer -> Integer
factorial n = factAux n 1

factAux :: Integer -> Integer -> Integer
factAux 0 r = r
factAux n r = factAux (n-1) (r*n)

-- > Generators     -----------------------------------------------------------
gen1 :: Gen Integer
gen1 = arbitrary `suchThat` (>= 0)

gen2 :: Gen Integer
gen2 = arbitrary `suchThat` (>= 1)

gen3 :: Gen Integer
gen3 = arbitrary `suchThat` (> 5)

-- > Property 1     -----------------------------------------------------------
prop_1 :: Integer -> Bool
prop_1 n | n == 0 = factorial 0 == 1
        | otherwise = factorial n == factorial (n-1) * n

-- prop1' :: Integer -> Property
-- prop1' n = n >= 0 ==> 
--     if n == 0 
--         then factorial 0 == 1
--     else factorial n == factorial (n-1) * n 

-- > Property 2     -----------------------------------------------------------
prop_2 :: Integer -> Bool
prop_2 n = fdiv (factorial n) n
    where
        fdiv n k | k == 0 = True
                 | mod n k /= 0 = False
                 | otherwise = fdiv n (k-1)

-- prop2' :: Integer -> Bool
-- prop2' n = n >= 0 ==> fdiv (factorial n) n
--     where
--         fdiv n k | k == 0 = True
--                  | mod n k /= 0 = False
--                  | otherwise = fdiv n (k-1)

-- > Property 3     -----------------------------------------------------------
prop_3 :: Integer -> Bool
prop_3 n = (a > fact) && (fact > b)
  where
    a = (fromIntegral n / 2)^n
    b = (fromIntegral n / 3)^n
    fact = fromIntegral (factorial n)

-- prop3' :: Integer -> Property
-- prop3' n = n > 5 ==> (a > fact) && (fact > b)
--   where
--     a = (fromIntegral n / 2)^n
--     b = (fromIntegral n / 3)^n
--     fact = fromIntegral (factorial n)

-- > Main            -----------------------------------------------------------     
main :: IO()
main = do
    putStrLn ">>> Testing Property 1:"
    quickCheck $ forAll gen1 prop_1

    putStrLn "\n>>> Testing Property 2:"
    quickCheck $ forAll gen2 prop_2

    putStrLn "\n>>> Testing Property 3:"
    quickCheck $ forAll gen3 prop_3

