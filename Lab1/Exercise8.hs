module Exercise8 where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck


-- Using Haskell to refute a conjecture. Write a Haskell function that can be used to refute the 
-- following conjecture. "If p1,...,pn is a list of consecutive primes starting from 2, then (p1×
-- ×pn)+1 is also prime." This can be refuted by means of a counterexample, so your Haskell 
-- program should generate counterexamples.

-- Steps:
-- "If p1,...,pn is a list of consecutive primes starting from 2, then then (p1x..xpn)+1 is also prime."
-- To refute this the below program will generate counter examples.
-- First it generates prime number list starting from 2.
-- E.g. [2], [2, 3], [2,3,5] etc
-- It creates product of the primes in each list and adds 1 (product + 1)
-- It checks weather the product plus 1 is prime or not.
-- If it is prime then it it will stores in list of tuples [[Prime number list], product of primes + 1)]

-- Generating natural numbers
genNat :: Gen Integer
genNat = arbitrary `suchThat` (>=1)

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

-- Generating Prime numbers
primeStream :: Integer -> [Integer]
primeStream x
  | x < 1 = []
  | otherwise = filter prime [1..x]

-- returns product + 1
productPlusOne :: [Integer] -> Integer
productPlusOne x = 1 + product x

-- Generate Counter examples of [2..50]
counterExamples ::  [([Integer], Integer)]
counterExamples = [ (primeStream x, productPlusOne (primeStream x)) | x <- [2..50], not (prime (productPlusOne (primeStream x)))]

-- The following list of consecutive primes refutes the statement
main = do
  putStrLn "The following list of consecutive primes refutes the statement - If p1,...,pn is a list of consecutive primes starting from 2, then then (p1x..xpn)+1 is also prime."
  print counterExamples
