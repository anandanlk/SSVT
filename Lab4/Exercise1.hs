module Exercise1 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

{-

---- EXERCISE 1---------------------------------------------------------------

> Time Spent: 1 hour

> Description:

    Implement a random data generator for the datatype Set Int , where Set is as defined in
    SetOrd.hs. First do this from scratch, next give a version that uses QuickCheck to random test
    this datatype.

> Solution

    The geneorator from scratch is the genSet' function. This function generates a max length that
    is between 0 and 10. It then calls the genInt function which takes the max length and then generates
    a list of Int of length maxLength. The genSet' function then uses the list2set function to turn the
    list into a set.

    The instance of arbitrary is the random data generator that uses the quickCheck library.

    We also added a genSet function that generates a generator using some of the functions from the quickCheck library.

-}

-- > Generator for sets without the use of the quickCheck library --------------------------------------------------------
genSet' :: IO (Set Int)
genSet' = do
    maxLength <- randomRIO (0, 10)
    xs <- genInt maxLength
    return $ list2set xs

genInt :: Int -> IO [Int]
genInt 0 = return []
genInt maxLength = do
    number <- randomIO :: IO Int
    rest <- genInt (maxLength - 1)
    return $ number:rest

-- > Generator for sets using the quickCheck library -----------------------------------------------------------------------
genSet :: (Arbitrary a, Ord a) => Gen (Set a)
genSet = do
    length <- choose (0, 10)
    list <- vectorOf length arbitrary
    return $ list2set list

-- > Arbitrary instance for sets --------------------------------------------------------------------------------------------
instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
    arbitrary = do
        length <- choose (0, 10)
        list <- vectorOf length arbitrary
        return $ list2set list

-- > Main --------------------------------------------------------------------------------------------------------------------
main = do
    putStrLn "Random generator for sets without using quickCheck library"
    result <- genSet'
    print result
    result <- genSet'
    print result
    putStrLn "Random generators for sets using quickCheck library"
    result <- generate (genSet :: Gen (Set Int))
    print result
    result <- generate (genSet :: Gen (Set String))
    print result
    putStrLn "Arbitrary for sets using quickCheck library"
    result <- generate (arbitrary :: Gen (Set Int))
    print result
    result <- generate (arbitrary :: Gen (Set String))
    print result
