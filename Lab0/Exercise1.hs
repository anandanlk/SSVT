module Exercise1 where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

{-
So first, the right part of the equality is just simple to write on 
Haskell. For the left part, we wanted to create the list, map it with an ^2
function and the sum all the components. For the right part, we just copied
the right formula into haskell.

To prove they are true, mathematically, we would need to use induction. However
QuickCheck allows us to intuit that the expression is correct from data. Why?
cause it only proves it for certain numbers, not for all of them.

 - Time Spent: 1 Hour
    Description: Exercise 1
     Write QuickCheck tests for the following statements:
     1^2 +2^2+⋅⋅⋅+n^2 =n(n+1)(2n+1)/6
     1^3 +2^3+⋅⋅⋅+n^3 =(n(n+1)/2)^2
     How would you prove that these statements are true for all natural numbers?
     Deliverables: Haskell program, indication of time spent.

 - Reasoning:
     To prove that these statements are true for all natural numbers, we can use induction.
        We can use the following steps:
            1. Prove that the statement is true for n = 1
            2. Assume that the statement is true for n = k
            3. Prove that the statement is true for n = k + 1

        For the first statement, we can use the following steps:

            1. Prove that the statement is true for n = 1
                1^2 = 1(1+1)(2*1+1)/6
                1 = 1(2)(3)/6
                1 = 1(6)/6
                1 = 1

            2. Assume that the statement is true for n = k
                1^2 + 2^2 + ⋅⋅⋅ + k^2 = k(k+1)(2k+1)/6

            3. Prove that the statement is true for n = k + 1
                1^2 + 2^2 + ⋅⋅⋅ + k^2 + (k+1)^2 = (k+1)((k+1)+1)(2(k+1)+1)/6

                -- Simplify the left side of the equation
                1^2 + 2^2 + ... + k^2 + (k+1)^2 = k(k+1)(2k+1)/6 + (k+1)^2

                -- Simplify the right side of the equation
                (k+1)((k+1)+1)(2(k+1)+1)/6 = (k+1)(k+2)(2k+3)/6

                -- Bring the new sides together
                k(k+1)(2k+1)/6 + (k+1)^2 = (k+1)(k+2)(2k+3)/6

                -- Factor out (k+1) from the left side
                (k+1)(k(2k+1)/6 + (k+1)) = (k+1)(k+2)(2k+3)/6

                -- Divide both sides by (k+1)
                k(2k+1)/6 + (k+1) = (k+2)(2k+3)/6

                -- Multiply both sides by 6
                k(2k+1) + 6(k+1) = (k+2)(2k+3)

                -- Simplify
                2k^2 + k + 6k + 6 = 2k^2 + 7k + 6

                -- Simplify
                2k^2 + 7k + 6 = 2k^2 + 7k + 6

                -- Therefore, the statement is true for n = k + 1

        For the second statement, a similar approach can be used:

            1. Prove that the statement is true for n = 1
                1^3 = (1(1+1)/2)^2
                1 = (1(2)/2)^2
                1 = (1)^2
                1 = 1

            2. Assume that the statement is true for n = k
                1^3 + 2^3 + ⋅⋅⋅ + k^3 = (k(k+1)/2)^2

            3. Prove that the statement is true for n = k + 1
                1^3 + 2^3 + ⋅⋅⋅ + k^3 + (k+1)^3 = ((k+1)((k+1)+1)/2)^2

                -- Simplify the left side of the equation
                1^3 + 2^3 + ... + k^3 + (k+1)^3 = (k(k+1)/2)^2 + (k+1)^3

                -- Simplify the right side of the equation
                ((k+1)((k+1)+1)/2)^2 = ((k+1)(k+2)/2)^2

                -- Bring the new sides together
                (k(k+1)/2)^2 + (k+1)^3 = ((k+1)(k+2)/2)^2

                -- Factor out (k+1)^2 from the left side
                (k+1)^2(k/2)^2 + (k+1)^3 = ((k+1)(k+2)/2)^2

                -- Factor out (k+1)^2 from the right side
                (k+1)^2(k/2)^2 + (k+1)^3 = (k+1)^2((k+2)/2)^2

                -- Divide both sides by (k+1)^2
                (k/2)^2 + (k+1) = ((k+2)/2)^2

                -- Multiply both sides by 4
                4(k/2)^2 + 4(k+1) = 4((k+2)/2)^2

                -- Simplify
                k^2 + 4k + 4 = (k+2)^2

                -- Simplify
                k^2 + 4k + 4 = k^2 + 4k + 4

                -- Therefore, the statement is true for n = k + 1

To test our implementation run the following command:
    
    quickCheck $ forAll genNat $ propExercise1b
-}

genNat :: Gen Int
genNat = arbitrary `suchThat` (>= 1)

propExercise1a :: Int -> Bool
propExercise1a n = sum (map (^2) [1..n]) == n * (n + 1) * (2*n + 1) `div` 6

propExercise1b :: Int -> Bool
propExercise1b n = sum (map (^3) [1..n]) == (n * (n + 1) `div` 2)^2


{-
Below is an example where we use guards to handle negative numbers. The solution should
use either guards or a natural number generator, but not both. We also feel that for this
example since we are trying to prove that the equation holds for all natural numbers, we 
should not handle negative numbers in the function but instead make sure the generator generates 
non negative numbers. The guards method also returns true for negative numbers since its outside 
the scope of this test but this is missleading since the function does not hold for negative numbers.


propExercise1a :: Int -> Bool
propExercise1a n 
    | n > 0 = sum (map (^2) [1..n]) == n * (n + 1) * (2*n + 1) `div` 6
    | otherwise = True

propExercise1b :: Int -> Bool
propExercise1b n 
    | n > 0 = sum (map (^3) [1..n]) == (n * (n + 1) `div` 2)^2
    | otherwise = True
-}

main :: IO()
main = return()