module Exercise1 where

import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable

{-

---- EXERCISE 1 ---------------------------------------------------------------

    > Time Spent: 2h

    > Description:

        Firstly, we order the mutators included in 'Mutation.hs' according to their
    strength level (we will go from the weakest to the strongest)

         1. anyList         weak mutator, as it has no constraints in terms of 
                            which list it can be applied to.
         2. addElements     weak mutator, as weak as the previous one, as it can
                            also be applied to any type of list.
         3. removeElements  weak mutator, but stronger than the previous ones, as in 
                            this case, if we apply it to an empty list we will not be 
                            mutating it.

        It is also worth noting that none of the given mutators consider outputs
    of fixed length. As the function to test ('multiplicationTable') always return a
    list of length 10, we thought it would be interesting to create some stronger 
    mutators which lenft the length of the constant. With that in mind, we thought
    of the following mutators:

         - replaceElements:     replaces from 1 to length elements by a random
                                value of an output list
         - multiplyElements:    multiply all elements by an arbitrary integer
         - reverseList:         reverses the order of the elements of an output
                                list
         - sortList:            sorts the elements of an output list
         - rotateList:          rotates the order of the elements by 1 to length
                                positions of an output list
         - suffleList:          suffle the elements of an output list

        However, we also came up with other ideas for mutators which modified
    the length:

         - addMidElements:      adds elements in a random position between the 
                                beginning and the end of an output list
         - removeDubElements:   removes duplicate elements of an output list
         - emptyList:           returns an empty list
         - randomList:          return a random list

-}

-- > Length Constant Mutator Functions  ---------------------------------------

-- Multiply with an arbitrary natural number. The length is fixed hence it is strong
-- but weaker than reverse or sort list
multiplyElements :: [Integer] -> Gen [Integer]
multiplyElements xs = do
    num <- arbitrary :: Gen Integer
    return (map (* num) xs)

-- Reverse the list. Content and length is same and order is different. Hence it is stronger
reverseList :: [Integer] -> Gen [Integer]
reverseList xs = return (reverse xs)

-- Sort the list. Content and length is same and order is different. Hence it is stronger
sortList :: [Integer] -> Gen [Integer]
sortList xs = return $ sort xs

-- All Random. Weaker than above all since it is completely random
allRandom :: [Integer] -> Gen [Integer]
allRandom xs = (arbitrary :: Gen [Integer]) `suchThat` (not . null)