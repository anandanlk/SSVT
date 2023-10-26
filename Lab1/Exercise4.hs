module Exercise4 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Exercise3 hiding (main)

{-

---- EXERCISE 4 ---------------------------------------------------------------

    > Time Spent: 4h

    > Description:

        We are asked to implement 'isDerangement', for that, we first implemented
    'isPermutation' function which is part of Bonus 1 exercise. 

    > Testing:

        We were unsure on how to test the function, so we started with some simple
    test to which we already knew the answer:

         - prop_more        We test if a list with more elements is a derangement from our 
                            list.
         - prop_less        We test if a list with less elements is a derangement from our
                            list.
         - prop_duplicate   We test if a list with duplicated elements is a derangement from
                            our list.
         - prop_odd         We test if the reverse of a odd length list is a derangement
                            from the original list.
         - prop_even        We test if the reverse of a even length list is a derangement
                            from the original list.

        We also considered using mathematical equalities, such as that the number
    of derrangements in a list is equals to the subfactorial of its lenght. That is

        |deran(n)| = !n

    where

        !n = (n-1)*(!(n-1) + !(n-2)) for n>=2

        However, we ended up discarding this type of properties as they required some
    extra code that would also need to be tested.

        All in all, we learned how difficult it really is to stop and think about
    generic properties that allow us to test our code. Also, it made us realise the 
    importance of using counter examples to test wether our implementation is correct
    or not.

    > Questions:

    4. Provide an ordered list of properties by strength using the weaker and stronger
    definitions.

        Now we provide an ordered list of all the defined properties as asked by 
    question 4 from the Exercise 4 instructions.

        [prop_even, prop_odd, prop_more, prop_less, prop_duplicate]

        Why this order? Basically we ordered them according to how specific their
    domain is. 'prop_even' and 'prop_odd' are not comparable between them, but still
    stronger than the rest of the properties as they apply to a subset of their domain.
    'prop_more', 'prop_less' and 'prop_duplicate' are also equally strong, as they all
    have the same domain.

    5. Can we automate the test process? 
    
        We have automated the test for the properties by using specific generators
    for each of them. However, if we try and order them using a similar code to the one
    on Exercise 3, we encounter the problem that Haskell's 'Ordering' does not provide
    a way to detect non-comparable stuff. It works, because non-comparable properties
    are considered 'equal', but this should not be considered a correct way of automating. 

-}

-- > Functions       ----------------------------------------------------------
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement xs ys = isPermutation xs ys && all (uncurry (/=)) (zip xs ys)

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = xs /= [] && ys /= [] && all (`elem` ys) xs && all (`elem` xs) ys

deran :: Int -> [[Int]]
deran n = filter (isDerangement [0..(n-1)]) (permutations [0..(n-1)])

-- > Generators ---------------------------------------------------------------
gen_1 :: Gen Int
gen_1 = arbitrary `suchThat` (>= 0)

gen_2 :: Gen Int
gen_2 = gen_1

gen_3 :: Gen Int
gen_3 = gen_1

gen_4 :: Gen Int
gen_4 = arbitrary `suchThat` (\x -> odd x && x >= 0)

gen_5 :: Gen Int
gen_5 = arbitrary `suchThat` even

-- > Properties     -----------------------------------------------------------
prop_more :: Int -> Bool
prop_more n = not $ isDerangement [1..n] (reverse [1..n+1])

prop_less :: Int -> Bool
prop_less n = not $ isDerangement [1..n] (reverse [1..n-1])

prop_duplicate :: Int -> Bool
prop_duplicate n = not $ isDerangement [1..n] (drop 1 [1..n] ++ [n])

prop_odd :: Int -> Bool
prop_odd n = not $ isDerangement [1..n] (reverse [1..n])

prop_even :: Int -> Bool
prop_even n = isDerangement [1..n] (reverse [1..n])

properties :: [(Int -> Bool, String)]
properties = [(prop_more,"prop_more"), (prop_less,"prop_less"), (prop_duplicate,"prop_duplicate"), (prop_odd,"prop_odd"), (prop_even,"prop_even")]

propertiesSorted :: [(Int -> Bool, String)]
propertiesSorted = sortBy (flip compareProperties) properties


-- > Sorting functions --------------------------------------------------------

compareProperties :: (Int -> Bool, String) -> (Int -> Bool, String) -> Ordering
compareProperties p q | stronger [0..10] (fst p) (fst q) && weaker [0..10] (fst p) (fst q) = EQ
                      | stronger [0..10] (fst p) (fst q) = GT
                      | otherwise = LT

printProperties :: [(Int -> Bool, String)] -> String
printProperties ps = intercalate "\n" (map snd ps)

-- > Main       ---------------------------------------------------------------
main :: IO()
main = do
    putStrLn ">>> Testing prop_more:"
    quickCheck $ forAll gen_1 prop_more

    putStrLn "\n>>> Testing prop_less:"
    quickCheck $ forAll gen_2 prop_less

    putStrLn "\n>>> Testing prop_duplicate:"
    quickCheck $ forAll gen_3 prop_duplicate

    putStrLn "\n>>> Testing prop_odd:"
    quickCheck $ forAll gen_4 prop_odd

    putStrLn "\n>>> Testing prop_even:"
    quickCheck $ forAll gen_5 prop_even

    putStrLn "\n>>> Unsorted Properties:"
    putStrLn $ printProperties properties

    putStrLn "\n>>> Sorted Properties:"
    putStrLn $ printProperties propertiesSorted