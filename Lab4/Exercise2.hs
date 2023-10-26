module Exercise2 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd
import Exercise1 hiding (main)

{-

---- EXERCISE 2---------------------------------------------------------------

> Time Spent: 2 hour

> Description:

    Implement operations for set intersection, set union and set difference, for the datatype Set
    defined in SetOrd.hs . Next, use automated testing to check that your implementation is
    correct. First use your own generator, next use QuickCheck.
    Use the following declarations:

> Solution:

    For an element to be in the intersection of two sets it must be in both original sets.
    The property we created looks at each element in the intersection and checks if that element
    exists in both original sets.

    For the setUnion all elements in the union of the sets must be in at least one of the original sets.
    The property looks at all elements in the final set and checks if that element is in one of the
    original sets
    We do not need to write a property for duplicate elements in the final sets because this is the
    responsibility of the list2set function not of the union function.

    The setDifference of set a and b contains all the elements that are in set a that are not in set b.
    The property looks at every element in the final set and cheacks that it exists in set a but not in
    set b.

    
    We added a gen2Sets function that generates two seperate sets and combines them in a tuple to make it
    easy to use with quickCheck. Our own version of the generator also uses this structure so that the
    properties work for both methods.

-}

-- > functions -------------------------------------------------------------------------------------------------

setIntersection :: Ord a => Set a -> Set a -> Set a 
setIntersection (Set xs) ys = Set (filter (\x -> inSet x ys) xs)

prop_setIntersection :: Ord a => (Set a, Set a) -> Bool
prop_setIntersection (xs, ys) = and $ map (\x -> (inSet x xs) && (inSet x ys)) ls
    where Set ls = setIntersection  xs ys

setUnion :: Ord a => Set a -> Set a -> Set a 
setUnion (Set xs) (Set ys) = list2set (xs ++ ys)

prop_setUnion :: Ord a => (Set a, Set a) -> Bool
prop_setUnion (xs, ys) = and $ map (\x -> (inSet x xs) || (inSet x ys)) ls
    where Set ls = setUnion xs ys

setDifference :: Ord a => Set a -> Set a -> Set a
setDifference (Set xs) ys = Set (filter (\x -> not (inSet x ys)) xs)

prop_setDifference :: Ord a => (Set a, Set a) -> Bool
prop_setDifference (xs, ys) = and $ map (\x -> (inSet x xs) && not (inSet x ys)) ls
    where Set ls = setDifference  xs ys

-- > Generators that generates two sets ------------------------------------------------------------------------------
gen2Sets :: (Ord a, Arbitrary a) => Gen (Set a, Set a)
gen2Sets = do
    set1 <- arbitrary
    set2 <- arbitrary
    return (set1, set2)

gen2Sets' :: IO (Set Int, Set Int)
gen2Sets' = do
    set1 <- genSet'
    set2 <- genSet'
    return (set1, set2)

-- > main ---------------------------------------------------------------------------------------------------------------
main :: IO ()
main = do
    putStrLn "Every element in the intersection of two sets should be in both original sets\
    \\nOwn generator"
    testSets <- gen2Sets'
    print $ prop_setIntersection testSets
    putStrLn "\nQuickCheck Generator"
    quickCheck $ forAll (gen2Sets :: Gen (Set Int, Set Int)) $ prop_setIntersection

    putStrLn "\n--------------------------------------------------------------------------------------------------------\
    \\n Every element in both original sets should be in the union of the sets\
    \\nOwn generator"
    testSets <- gen2Sets'
    print $ prop_setUnion testSets
    putStrLn "\nQuickCheck Generator"
    quickCheck $ forAll (gen2Sets :: Gen (Set Int, Set Int)) $ prop_setUnion

    putStrLn "\n--------------------------------------------------------------------------------------------------------\
    \\nEvery element in the first set that is not in the second set should be in the set difference of the sets\
    \\nOwn generator"
    testSets <- gen2Sets'
    print $ prop_setDifference testSets
    putStrLn "\nQuickCheck Generator"
    quickCheck $ forAll (gen2Sets :: Gen (Set Int, Set Int)) $ prop_setDifference


-- No longer needed
-- setUnionMinIntersection x@(Set xs) y@(Set ys) = list2set (filter (\z -> not ((inSet z x) && (inSet z y))) (xs ++ ys))