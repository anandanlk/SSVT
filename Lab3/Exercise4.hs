module Exercise4 where
import MultiplicationTable
import Mutation
import Exercise2 hiding (main)
import Test.QuickCheck hiding (property)


{-

---- EXERCISE 4 ---------------------------------------------------------------

> Time Spent: 3h

> Description:
Implement a function that calculates the strength of a given set of properties, which is the
percentage of mutants they kill.

-}

{-
Reusing the below functions from Exercise 2.

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

-}

-- Calculate the percentage of mutants killed by a set of properties
-- Calculating for 4000 tests
strength :: ([Integer] -> Gen [Integer]) -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> Integer -> Gen Integer
strength m p f i = do
    survivors <- countSurvivors 4000 m p f i
    return $ ((4000 - survivors) * 100 ) `div` 4000

    -- Calculating the percentage:
    -- Mutants killed = Total Tests - Survivors
    -- Mutants killed percentage = Mutants killed/total tests * 100

main :: IO ()
main = do
    putStrLn "--------------------------------------------------------------------------------"
    putStrLn "The percentage of mutants killed by properties (i.e. Strength of the properties):"
    putStrLn "--------------------------------------------------------------------------------"

    putStr "1.1. Mutator: removeElements, Property: prop_tenElements                     :"
    percentage <- generate $ strength removeElements [prop_tenElements] multiplicationTable 5
    putStr (show percentage)

    putStr "%\n1.2. Mutator: removeElements, Property: prop_firstElementIsInput             :"
    percentage <- generate $ strength removeElements [prop_firstElementIsInput] multiplicationTable 5
    putStr (show percentage)

    putStr "%\n1.3. Mutator: removeElements, Property: prop_sumIsTriangleNumberTimesInput   :"
    percentage <- generate $ strength removeElements [prop_sumIsTriangleNumberTimesInput] multiplicationTable 5
    putStr (show percentage)

    putStr "%\n1.4. Mutator: removeElements, Property: prop_linear                          :"
    percentage <- generate $ strength removeElements [prop_linear] multiplicationTable 5
    putStr (show percentage)

    putStr "%\n1.5. Mutator: removeElements, Property: prop_moduloIsZero                    :"
    percentage <- generate $ strength removeElements [prop_moduloIsZero] multiplicationTable 5
    putStr (show percentage)

    putStr "%\n2.1. Mutator: addElements,    Property: prop_tenElements                     :"
    percentage <- generate $ strength addElements [prop_tenElements] multiplicationTable 5
    putStr (show percentage)

    putStr "%\n2.2. Mutator: addElements,    Property: prop_firstElementIsInput             :"
    percentage <- generate $ strength addElements [prop_firstElementIsInput] multiplicationTable 5
    putStr (show percentage)

    putStr "%\n2.3. Mutator: addElements,    Property: prop_sumIsTriangleNumberTimesInput   :"
    percentage <- generate $ strength addElements [prop_sumIsTriangleNumberTimesInput] multiplicationTable 5
    putStr (show percentage)

    putStr "%\n2.4. Mutator: addElements,    Property: prop_linear                          :"
    percentage <- generate $ strength addElements [prop_linear] multiplicationTable 5
    putStr (show percentage)

    putStr "%\n2.5. Mutator: addElements,    Property: prop_moduloIsZero                    :"
    percentage <- generate $ strength addElements [prop_moduloIsZero] multiplicationTable 5
    putStr (show percentage)

    putStr "%\n3.1. Mutator: anyList,        Property: prop_tenElements                     :"
    percentage <- generate $ strength anyList [prop_tenElements] multiplicationTable 5
    putStr (show percentage)

    putStr "%\n3.3. Mutator: anyList,        Property: prop_sumIsTriangleNumberTimesInput   :"
    percentage <- generate $ strength anyList [prop_sumIsTriangleNumberTimesInput] multiplicationTable 5
    putStr (show percentage)

    putStr "%\n3.4. Mutator: anyList,        Property: prop_linear                          :"
    percentage <- generate $ strength anyList [prop_linear] multiplicationTable 5
    putStr (show percentage)

    putStr "%\n3.5. Mutator: anyList,        Property: prop_moduloIsZero                    :"
    percentage <- generate $ strength anyList [prop_moduloIsZero] multiplicationTable 5
    putStr (show percentage)
    putStrLn "%"