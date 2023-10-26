module Exercise2 where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
    p <- getStdRandom random
    ps <- probs (n-1)
    return (p:ps)

{-

Time spent: 3 hours

Description:
Our first idea is to count the amount of elements that belong to each of the
quartiles. For that we basically need to filter them in some way. Here the first
difficulty arrises, as we are working with monads. That's why we created
the counter function, where we unwrap the IO [Float] in order to count the elements
that belong to each of the quartiles.

To prove the correctness of our program we created a proposition that uses the maximum
standard deviation to check wether or not the error rate of each quartile is acceptable.
We can run this proposition using the testrunner function. This function takes in the 
number of tests you want to do and then prints the amount of success there were. 
When we run this function we see that we have a rougly 85% success rate. The command
to run the test is the following:

    testrunner m 

-}

counter :: IO [Float] -> IO [Int]
counter xs = do
    x <- xs
    return (quartiles x)
    where
        quartiles xs = [
            length (filter (<= 0.25) xs),
            length (filter (\x -> x > 0.25 && x <= 0.5) xs),
            length (filter (\x -> x > 0.5 && x <= 0.75) xs),
            length (filter (> 0.75) xs)
            ]

propExercise2 :: Int -> IO Bool
propExercise2 n = do
    qs <- counter $ probs n
    let error = (sqrt ((((fromIntegral n) - (fromIntegral n / 4))^2) + (3 * ((fromIntegral n / 4)^2)))) / sqrt ((fromIntegral n)-1)
    return $ foldr (f error) True qs
    where
        f error next prev = prev && (abs(fromIntegral next - (fromIntegral n / 4)) <= error)

testrunner m = do
    props <- mapM propExercise2 [1..m]
    let result = length (filter ((==) True) props)
    return result

main :: IO()
main = return()