module Exercise2 where
import MultiplicationTable
import Mutation
import Test.QuickCheck

{-

---- EXERCISE 2 ---------------------------------------------------------------

> Time Spent: 3h

> Description:
Write a function countSurvivors that counts the number of survivors:

ountSurvivors :: Integer -> [([Integer] -> Integer -> Property)] -> (Integer -> [Inte
ger]) -> Integer

Where the first argument is the number of mutants (4000 in the FitSpec example), the second
argument is the list of properties, and the third argument is the function under test (the
multiplication table function in this case).
The output is the number of surviving mutants (0 in the FitSpec example).
Document the effect of which mutations are used and which properties are used on the
number of survivors.

-}

-- Count the survivors for n times using recursive function
-- n - no. of tests, m - mutator, p - proerties
-- f - function (here multiplicationTable), i - input to the fucntion multiplicationTable
countSurvivors :: Integer -> ([Integer] -> Gen [Integer]) -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> Integer -> Gen Integer
countSurvivors 0 _ _ _ _ = return 0
countSurvivors n m p f i = do
    mf <-  m (f i)
    s <- countSurvivors (n-1) m p f i
    return $ if survives p mf i then s+1 else s  -- Summing up the survivors for each test

-- Return True if the mutant survives for the given property
-- p, px- Property(ies), mf - mutated function
-- Aggregating the survivor result by applying properties one by one
survives :: [[Integer] -> Integer -> Bool] -> [Integer] -> Integer -> Bool
survives (p:px) mf i = p mf i && survives px mf i
survives [] _ _ = True

-- Reusing the properties from MutationTable
properties :: [[Integer] -> Integer -> Bool]
properties = [prop_tenElements, prop_firstElementIsInput,
    prop_sumIsTriangleNumberTimesInput, prop_linear, prop_moduloIsZero]

-- Execute and count the survivors for 4000 tests
-- Reusing the muartions from Mutation.hs
main :: IO ()
main = do
    putStrLn "Number of survivors out of 4000 for the mutator 'addElements': "
    survivors <- generate $ countSurvivors 4000 addElements properties multiplicationTable 5
    print survivors
    putStrLn "Number of survivors out of 4000 for the mutator 'removeElements': "
    survivors <- generate $ countSurvivors 4000 removeElements properties multiplicationTable 5
    print survivors
    putStrLn "Number of survivors out of 4000 for the mutator 'anyList': "
    survivors <- generate $ countSurvivors 4000 anyList properties multiplicationTable 5
    print survivors