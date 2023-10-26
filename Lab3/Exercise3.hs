{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Exercise3 where

import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable
import Debug.Trace

import Exercise2 hiding (main)

{-

---- EXERCISE 3 ---------------------------------------------------------------

> Time Spent: 12h

> Description:
        implement a function that calculates the minimal property subsets, given a 'function under
    test' and a set of properties.

        To implement this function we had the idea of generating all subsets of a set of 
    properties and then comparing the subsets to the original set to see if any subsets kill 
    just as many mutants.

    Example:
    {1,2,3} 4000
        |
       ------------------------   (Direct subsets)
      |            \           \
    {1,2},         {1,3},      {2,3}
    4000           4000        4000
      |             |            |
      -------       ------       -------  
      |      |      |     |      |      |
    {1},    {2},   {1},  {3}   {2},    {3}
    1200    3600   3200  4000  1000     4000

    The sets after running the minPropSet' function would be
    [{1,2},{1,3},{2,3},{3},{3}]
    
    The minPropSet function would then filter out any duplicates and only keep the smallest sized sets.
    [{3}]
    In this example the property set {3} is the minimal property set. This does not always need to be a
    set of size 1 it could be any of the subsets of the original set.

-}

-- A direct subsequence is all sub sequences of list xs with length xs - 1
directSubsequences :: [(String, [Integer] -> Integer -> Bool)] -> [[(String, [Integer] -> Integer -> Bool)]]
directSubsequences xs = map (removeAtIndex xs) [1..(length xs)]

-- This function removes a value in a list at index i
removeAtIndex xs i = take (i-1) xs ++ drop i xs

-- This is the first function that gets called.
-- 1. This function first creates a list of all possible property subsets that kill just as many mutants as the base set.
-- 2. It should then get the smallest size of set and filter out any sets that are larger than the smallest set.
-- Since we could not get the minPropSet' function (step 1.) to work we also did not finish this function.
minPropSet :: Integer -> [[Integer] -> Gen [Integer]] -> [(String, [Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> [[(String, [Integer] -> Integer -> Bool)]]
minPropSet n mutators props fut = minPropSet' n mutators [props] fut base
    where base = countAllSurvivors n mutators (map snd props) fut

-- This function generates all the subsets of a set of properties and keeps those that are just as strong as the base property set.
-- This function was really close to working but then we hit some problems with Monads and it ended up becoming to messy and complicated.
minPropSet' :: Integer -> [[Integer] -> Gen [Integer]] -> [[(String, [Integer] -> Integer -> Bool)]] -> (Integer -> [Integer]) -> Gen Integer -> [[(String, [Integer] -> Integer -> Bool)]]
minPropSet' n mutators [] fut base = do return []
minPropSet' n mutators (p:ps) fut base | not (null subs) = do
    subs <- subs
    return $ minPropSet' n mutators (map snd subs) fut base ++ minPropSet' n mutators ps fut base
                                       | otherwise       = return $ p : minPropSet' n mutators ps fut base
    where
        zipper = zip (map subCompareToBase (directSubsequences p)) (directSubsequences p)
        subs = do
            let (l, r) = unzip zipper
            l' <- l
            let rezip = zip l' r
            return $ filter fst rezip
        subCompareToBase subp = do
            survivors <- countAllSurvivors n mutators (map snd subp) fut
            b <- base
            return $ b >= survivors

main :: IO()
main = do
    print $ map (map fst) (minPropSet 4000 [addElements,removeElements] props multiplicationTable)

props = [
        ("prop_tenElements", prop_tenElements),
        ("prop_firstElementIsInput", prop_firstElementIsInput),
        ("prop_sumIsTriangleNumberTimesInput", prop_sumIsTriangleNumberTimesInput),
        ("prop_linear", prop_linear),
        ("prop_moduloIsZero", prop_moduloIsZero)
    ]




countAllSurvivors :: Integer -> [[Integer] -> Gen [Integer]] -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> Gen Integer
countAllSurvivors n mutators props fut = do
    survivorCounts <- mapM (\mutator -> countSurvivors n mutator props fut 2) mutators
    return $ sum survivorCounts