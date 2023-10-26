module Exercise3 where
    
import Data.List
import System.Random
import Test.QuickCheck

{-

---- EXERCISE 3 ---------------------------------------------------------------

> Time Spent: 45 mins

> Description:

Suppose we implement binary relations as list of pairs, Haskell type [(a,a)]. Assume the
following definition:
type Rel a = [(a,a)]

Use the following declaration:
symClos :: Ord a => Rel a -> Rel a
to define a function that gives the symmetric closure of a relation, where the relation is
represented as an ordered list of pairs. E.g., symClos [(1,2),(2,3),(3,4)] should give 
[(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)].

Deliverables: Haskell program, indication of time spent.

-}


type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos ((x,y):rs) = nub $ (x,y) : (y,x) : symClos rs
-- Take the first set (x,y) and merge it with (y,x) and recursively call the rest of the sets

main :: IO ()
main = do 
    putStr "symClos [(1,2),(2,3),(3,4)] = "
    print (symClos [(1,2),(2,3),(3,4)])