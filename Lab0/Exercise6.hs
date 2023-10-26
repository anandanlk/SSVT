module Exercise6 where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

{-
Time spent: 30 mins

Description:
So our initial idea was to basically go through the list of prime numbers adding 101 consecutive primes
and checking whether or not their sum was a prime number. That's why we used the search function, which
takes the amount of adding ups (n) and a counter (c). We start at the begining of the primes list, taking
the first 101 prime numbers. We check if their sum is prime: if it is, it means we are done; if it is not,
then we call the function again, this time starting from the second prime (that's why we increment c in 1).

Now we have a way of calculating the smallest prime number formed by the sum of "n" consecutive prime
numbers. However, we still need to check wether or not this function is correct. For now, we don't know
how to test if this function is correct or not.
-}

searchSmallestPrime :: Integer -> Integer
searchSmallestPrime n = search n 0
    where 
        search m c 
            | prime (sum (take (fromIntegral m) (drop (fromIntegral c) primes))) = sum (take (fromIntegral m) (drop (fromIntegral c) primes))
            | otherwise = search m (c + 1)

consecutive101Prime :: Integer
consecutive101Prime = searchSmallestPrime 101

main :: IO()
main = return()
