{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Exercise5 where

import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable
import Debug.Trace
import Exercise2 hiding (main)

{-

---- EXERCISE 5 ---------------------------------------------------------------

    > Time Spent: 3h

    > Description:

        We calculate the conjectures of a given property list. To do that, we 
    use the following approach:
    
        1. Firstly, we count for every subsequence of the given property list the 
           number of survivors.
        2. Then, we check for equivalences and implications in between all of the subsequences.
            a. In case of equivalences, we just check if two properties have the same
               number of survivors. If they have different number, it means they cannot 
               be equal.
            b. In case of implications, we check if one property kills less or equal
               than annotationanother. If one property kills more than the other, it means that it
               also covers the case of the other property. Therefore, it implies it.
        3. We print a list that contains tuples where the first element is the name
           of a property and the second element is a list with the names of the properties
           it is equivalent/implies
-}

-- > Auxiliary Functions    ---------------------------------------------------
countAllSurvivors :: Integer -> [[Integer] -> Gen [Integer]] -> [(String, [Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> Gen Integer
countAllSurvivors n mutators props fut = do
    input <- arbitrary :: Gen Integer
    survivorCounts <- mapM (\mutator -> countSurvivors n mutator props' fut input) mutators
    return $ sum survivorCounts
    where
        props' = map snd props

-- > Functions  ---------------------------------------------------------------

-- We count for each posible subsequence of properties the amount of survivors
-- it has. We will use this to compare.
conjectures :: Integer -> [[Integer] -> Gen [Integer]]  -> [(String, [Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> Gen [(String, Integer)]
conjectures _ _ [] _ = return []
conjectures m mutators props fut = do
    propsSurvivors <- mapM (\p -> countAllSurvivors m mutators p fut) (subsequences props)
    return $ zip (map concat (subsequences (map fst props))) propsSurvivors

-- We check for equivalences in a given list of properties
eq :: [(String, Integer)] -> [(String, [String])]
eq props = [(nx,x) | (nx,x) <- equivalence props, not $ null x]

equivalence :: [(String, Integer)] -> [(String, [String])]
equivalence ((nx,x):[]) = [(nx,[])]
equivalence ((nx,x):(ny,y):ps) = equivalence' (nx,x) ((ny,y):ps) [] : equivalence ((ny,y):ps)

equivalence' :: (String, Integer) -> [(String, Integer)] -> [String] -> (String,[String])
equivalence' (nx,x) [] res = (nx,res)
equivalence' (nx,x) ((ny,y):ps) res | x == y = equivalence' (nx,x) ps (ny:res)
                                    | otherwise = equivalence' (nx,x) ps res

-- We check for implications in a given list of properties
im :: [(String, Integer)] -> [(String, [String])]
im props = [(nx,x) | (nx,x) <- implication props, not $ null x]

implication :: [(String, Integer)] -> [(String, [String])]
implication ((nx,x):[]) = [(nx,[])]
implication ((nx,x):(ny,y):ps) = implication' (nx,x) ((ny,y):ps) [] : implication ((ny,y):ps)

implication' :: (String, Integer) -> [(String, Integer)] -> [String] -> (String,[String])
implication' (nx,x) [] res = (nx,res)
implication' (nx,x) ((ny,y):ps) res | x <= y = implication' (nx,x) ps (ny:res)
                                    | otherwise = implication' (nx,x) ps res

-- > Main   -------------------------------------------------------------------

mutatrs = [
        addElements,
        removeElements
    ]
props = [
        ("prop_tenElements", prop_tenElements),
        ("prop_firstElementIsInput", prop_firstElementIsInput),
        ("prop_sumIsTriangleNumberTimesInput", prop_sumIsTriangleNumberTimesInput)
        -- ("prop_linear", prop_linear),
        -- ("prop_moduloIsZero", prop_moduloIsZero)
    ]

main :: IO()
main = do 
    eq <- generate testEquivalence
    im <- generate testImplication
    putStrLn ">>> Testing equivalence:"
    print eq
    putStrLn ">>> Testing implication:"
    print im

-- We calculate the equivalences of the set of properties
testEquivalence :: Gen [(String,[String])]
testEquivalence = do
    propsSurvivors <- conjectures 1 mutatrs props multiplicationTable
    return $ eq propsSurvivors

-- We calculate the implications of the set of properties
testImplication :: Gen [(String,[String])]
testImplication = do
    propsSurvivors <- conjectures 1 mutatrs props multiplicationTable
    return $ im propsSurvivors
