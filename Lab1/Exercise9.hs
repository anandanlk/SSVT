module Exercise9 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

{-

---- EXERCISE 9 (BONUS) -------------------------------------------------------

    > Time Spent: 4h

    > Description:

        We are asked to implement 'isPermutation', which we programmed for
    Exercise 4.

    > Testing:

        We were unsure on how to test the function, so we started with some simple
    test to which we already knew the answer:

         - prop_more        We test if a list with more elements is a permutation from our 
                            list.
         - prop_less        We test if a list with less elements is a permutation from our
                            list.
         - prop_duplicate   We test if a list with duplicated elements is a permutation from
                            our list.
         - prop_reverse     We test if the reverse of a list is a permutation of the original
                            list

        All in all, we learned how difficult it really is to stop and think about
    generic properties that allow us to test our code. Also, it made us realise the 
    importance of using counter examples to test wether our implementation is correct
    or not.

    > Questions:

    1. You may assume that your input lists do not contain duplicates. What
    does this mean for your testing procedure?

        This means all test should be on list of the form [1..n], which will
    guarantee that there are no duplicates.

    2. Can you automate the test process? Also, use QuickCheck.

        We have used quickCheck to check all the previously explained properties.
    However, as in the rest of the exercises, we defined specific generators for
    each of them, as they all have different domains.

-}

-- > Function       -----------------------------------------------------------
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = xs /= [] && ys /= [] && all (`elem` ys) xs && all (`elem` xs) ys

-- > Generators ---------------------------------------------------------------
gen_1 :: Gen Int
gen_1 = arbitrary `suchThat` (>= 1)

gen_2 :: Gen Int
gen_2 = gen_1

gen_3 :: Gen Int
gen_3 = arbitrary `suchThat` (>= 2)

gen_4 :: Gen Int
gen_4 = gen_1

-- > Properties     -----------------------------------------------------------
prop_more :: Int -> Bool
prop_more n = not $ isPermutation [1..n] [1..n+1]

prop_less :: Int -> Bool
prop_less n = not $ isPermutation [1..n] [1..n-1]

prop_duplicate :: Int -> Bool
prop_duplicate n = not $ isPermutation [1..n] (drop 1 [1..n] ++ [n])

prop_reverse :: Int -> Bool
prop_reverse n = isPermutation [1..n] (reverse [1..n])

-- > Main       ---------------------------------------------------------------
main :: IO()
main = do
    putStrLn ">>> Testing prop_more:"
    quickCheck $ forAll gen_1 prop_more

    putStrLn "\n>>> Testing prop_less:"
    quickCheck $ forAll gen_2 prop_less

    putStrLn "\n>>> Testing prop_duplicate:"
    quickCheck $ forAll gen_3 prop_duplicate

    putStrLn "\n>>> Testing prop_reverse:"
    quickCheck $ forAll gen_4 prop_reverse

