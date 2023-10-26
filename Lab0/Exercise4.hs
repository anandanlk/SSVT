module Exercise4 where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

{-

Time spent: 1 hour

Description:
The natural number 13 has the property that it is prime and its reversal, the number 31, is also 
prime. Write a function that finds all primes < 10000 with this property

For this exercise we first create a list of all primes below 10000. Then we reverse all the primes
and filter out all the reversed primes that are not prime numbers. Then we reverse the list again.
This leaves us with a list off all reversible prime numbers. To test if the function works we have
defined 4 properties.

-}

reversal :: Integer -> Integer
reversal = read . reverse . show

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..9999]

reversibleStream :: [Integer]
reversibleStream = map reversal (filter prime (map reversal primes))

-- Reversible Prime Count:
-- From research we know that there are 240 emprimes that are smaller than 10000 these are prime numbers 
-- that when reversed are a different prime number. This 240 does not include palindromic primes.
-- From research we have also found that there are 20 palindromic primes that are smaller than 10000.
-- This means there are a total of 260 reversible primes

reversiblePrimeCount :: Bool
reversiblePrimeCount = 260 == length reversibleStream

-- Reversal Symmetry:
-- This property makes sure that if a prime is in the final list then the reverse of that prime is also in the list.
-- We confirm this by checking if the reverse of all the primes in the final list are in the final list.

reversalSymmetry :: Bool
reversalSymmetry = all (f . reversal) reversibleStream
  where f x = x `elem` reversibleStream

-- Unique Values:
-- This function takes every element in a list one by one and looks if that element appears in the remainder of the list.

uniqueValues :: [Integer] -> Bool
uniqueValues [] = True
uniqueValues (x:xs) = x `notElem` xs && uniqueValues xs

-- Check Maximum Value:
-- This function just makes sure that all values in the final primes list are smaller than 10000.

checkMaxVal :: Bool
checkMaxVal = all (10000 >) reversibleStream

-- This function checks if all properties hold.
propertyChecks :: Bool
propertyChecks = reversiblePrimeCount && reversalSymmetry && uniqueValues reversibleStream && checkMaxVal

main :: IO()
main = return()