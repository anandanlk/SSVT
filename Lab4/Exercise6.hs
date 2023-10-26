module Exercise6 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd
import Exercise3 hiding (main)
import Exercise4 hiding (main, Rel)
import Exercise5 hiding (main, Rel)

{-

---- EXERCISE 6 --------------------------------------------------------------

    > Time Spent: 1.5h

    > Description:

    We need to test the functions 'symClos' and 'trClos'. Preferably, we should
    use random test generation. For that reason, we have stablished a list
    of properties which we believe suffice to test for a correct implementation.
    This properties allow us to use QuickCheck by defining especific generators
    for each of them. We will further explain this in the Test section.

    > Test:

    Here we list all the programmed properties. For each of them, we give a brief
    of explanation and we reason on the generator used for the test.

    prop_simRel             If a relation is already simmetrical, xs == symClos xs
                            should hold. For this property, we have created 'genSymRel'
                            that generates symmetrical relations.
    prop_simExtractElem     If a relation is already simmetrical and we take out one
                            element, xs == symClos (drop 1 xs) should not hold. For 
                            this property we also use 'genSymRel'.
    prop_simLen             The length of the symmetrical closure should be smaller or
                            eq than twice the length of the original list. For this
                            property we use 'genRel'', which generates an arbitrary 
                            non-empty relation (not neccesarily symmetrical).
    prop_trRel              If a relation is already transtive, xs == trClos xs
                            should hold. For this property, we have created 'genTrRel'
                            that generates transitive relations.
    prop_trExtractElem      If a relation is already transitive and we take out one
                            element, xs == trClos (drop 1 xs) should not hold. For 
                            this property we also use 'genTrRel'.
    prop_trLen              The length of the transitive closure should be bigger or eq
                            to the length of the original list
    prop_simtrSelves        If the relation is of the type [(a,a)], meaning that each
                            element is only related to themselves, then it should hold
                            that the relation is symmetrical and transitive.
-}

-- > Generators ---------------------------------------------------------------
genRel' :: Gen (Rel Int)
genRel' = do
    length <- choose (1, 10)
    l <- vectorOf length arbitrary
    r <- vectorOf length arbitrary
    return $ (sort . nub) $ zip l r

genSymRel :: Gen (Rel Int)
genSymRel = do symClos <$> genRel'

genTrRel :: Gen (Rel Int)
genTrRel = do trClos <$> genRel'

genSelvesRel :: Gen (Rel Int)
genSelvesRel = do
    length <- choose (1,10)
    x <- vectorOf length arbitrary
    return $ (sort . nub) $ zip x x

-- > Auxiliary Functions    ---------------------------------------------------
isSym :: Ord a => Rel a -> Bool
isSym xs = xs == symClos xs

isTr :: Ord a => Rel a -> Bool
isTr xs = xs == trClos xs

-- > Properties ---------------------------------------------------------------
-- If a relation is already simmetrical, xs == symClos xs should hold
prop_simRel :: (Eq a, Ord a) => Rel a -> Bool
prop_simRel xs = xs == symClos xs

-- If a relation is already simmetrical and we we take one element, 
-- xs == symClos (drop 1 xs) should not hold
prop_simExtractElem :: (Eq a, Ord a) => Rel a -> Bool
prop_simExtractElem xs = xs /= symClos (drop 1 xs)

-- The length of the symetrical closure should be smaller or eq
-- than twice the length of the original list
prop_simLen :: (Eq a, Ord a) => Rel a -> Bool
prop_simLen xs = 2 * length xs >= length (symClos xs)

-- If a relation is already transitive, xs == trClos xs should hold
prop_trRel :: (Eq a, Ord a) => Rel a -> Bool
prop_trRel xs = xs == trClos xs

-- If a relation is already transitive and we take out one 
-- of the elements it, xs == trClos (drop 1 xs) should not hold
prop_trExtractElem :: (Eq a, Ord a) => Rel a -> Bool
prop_trExtractElem xs = xs /= trClos (drop 1 xs)

-- The length of the transitive closure should be bigger or eq
-- to the length of the original list
prop_trLen :: (Eq a, Ord a) => Rel a -> Bool
prop_trLen xs = length xs <= length (trClos xs)

-- If a relation is only composed of elements related to them selves,
-- the relation should be both symetrical and transitive
prop_symtrSelves :: (Eq a, Ord a) => Rel a -> Bool
prop_symtrSelves xs = isSym xs && isTr xs

-- > Main   -------------------------------------------------------------------
main :: IO()
main = do
    putStrLn ">>> Testing prop_simRel:"
    quickCheck $ forAll genSymRel prop_simRel
    putStrLn ">>> Testing prop_simExtractElem:"
    quickCheck $ forAll genSymRel prop_simExtractElem
    putStrLn ">>> Testing prop_simLen:"
    quickCheck $ forAll genRel prop_simLen
    putStrLn ">>> Testing prop_trRel:"
    quickCheck $ forAll genTrRel prop_trRel
    putStrLn ">>> Testing prop_trExtractElem:"
    quickCheck $ forAll genSymRel prop_trExtractElem
    putStrLn ">>> Testing prop_trLen:"
    quickCheck $ forAll genRel prop_trLen
    putStrLn ">>> Testing prop_symtrSelves:"
    quickCheck $ forAll genSelvesRel prop_symtrSelves