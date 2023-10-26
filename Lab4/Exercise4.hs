module Exercise4 where
    
import Data.List
import System.Random
import Test.QuickCheck

{-

---- EXERCISE 3 ---------------------------------------------------------------

    > Time Spent: 2 hour

    > QUESTION 1:

        > Description:

            Write a function for checking whether a relation is serial.
            Test your implementation with two QuickCheck properties.

        > Solution:

            For a relation to be serial every element in the domain must have a relation from
            that element to another element that is also in the domain.
            
            The isSerial function loop over all the elements in the domain, it then tries to find
            a relation from that element to another element in the domain. If it cannot find this
            relation for each element in the domain then it returns False else it returns True.

            The first property checks that there is an accompanying relationship for each element
            in the domain.
            The second property gets all relations linked to the elements in the domain. This is a
            [[Rel a]]. For each of these [Rel a] it checks if one of the secocd elements in the
            relation are in the domain.

            For the quickCheck tests sometimes too many test would be discarded and the test would
            exit, but this limit was so close to the standard limit that we felt it unnecesary to
            change the generator. Instead we increassed the limit slightly to allow for a few more
            tests to be discarded.


    > QUESTION 2:
        > Descrition:

            Consider the relation R = {(x, y) | x = y(mod n)}, where (mod n) is the modulo function
            in modular arithmetic and n > 0. Discuss whether (and when) R is serial. How can you
            test whether R is serial? How can you prove that R is serial?

        > Solution

            The modular arithmethic is an 'equivalence relationship'. 
            Therefore, it verifies the following properties:
            
            Reflexive       a = a (mod n)
            Symmetrical     a = b (mod n) <=> b = a (mod n)
            Transitive      a = b (mod n) and b = c (mod n), then a = c (mod n)

            This exercise wants us to discuss wether the modular arithmethic
            is serial or not. Our initial hypothesis is that it is always serial,
            no matter which n is chosen. To prove that we have followed the
            following reasoning:

            Let's consider our domain A = N (Natural numbers). Let's now fix an
            n = 2 as the base of our module. In this case, the modular arithmethic
            creates two equivalence classes (denoted by [_]):

            [0] := 0,2,4,6,8...
            [1] := 1,3,5,7,9...

            For that reason, for every x ∈ N there always exists an y ∈ N such that
            x = y (mod n). For example, if x = 3 then y ∈ [1], ie, it can take any
            value from the class [1] (1,3,5,7,...,231,...). 

            This that has been observed for n = 2 can be proven by induction for bigger
            values of n. Therefore, we conclude that the modular arithmethic is serial.
-}

-- > Auxiliary functions ----------------------------------------------------------------------------------------

-- From Data.List.Utils
{- | Returns true if the given list contains any of the elements in the search
list. -}
hasAny :: Eq a => [a]           -- ^ List of elements to look for
       -> [a]                   -- ^ List to search
       -> Bool                  -- ^ Result
hasAny [] _          = False             -- An empty search list: always false
hasAny _ []          = False             -- An empty list to scan: always false
hasAny search (x:xs) = if x `elem` search then True else hasAny search xs

type Rel a = [(a,a)]

-- > Implementation ---------------------------------------------------------------------------------------------

isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial xs ys = and $ map (\x -> f x ys) xs
    where f x [] = False
          f x ((a,b):ys) | x == a && elem b xs = True
                         | otherwise = f x ys

-- > Properties ------------------------------------------------------------------------------------------------

prop_firstRelElement :: Eq a => [a] -> Rel a -> Property
prop_firstRelElement xs ys = isSerial xs ys ==> and $ map (\x -> elem x (map fst ys)) xs

prop_secondRelElement :: Eq a => [a] -> Rel a -> Property
prop_secondRelElement xs ys = isSerial xs ys ==> and $ map (hasAny xs) ((map (getListRel ys) xs)) -- all b's linked to the a's

getListRel :: Eq a => Rel a -> a -> [a]
getListRel ys x = map snd (filter (\y -> x == (fst y)) ys)

-- > Generators ------------------------------------------------------------------------------------------------

genRel :: Gen (Rel Int)
genRel = do
    length <- choose (0, 10)
    l <- vectorOf length arbitrary
    r <- vectorOf length arbitrary
    return $ zip l r

genDomain :: Gen [Int]
genDomain = do
    length <- choose (0, 10)
    vectorOf length arbitrary

genRelDomainTuple :: Gen ([Int],Rel Int)
genRelDomainTuple = do 
    domain <- genDomain
    rel <- genRel
    return (domain, rel)

-- > Main ------------------------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "Checks that every element in the domain has an accompanieing rel"
    quickCheckWith stdArgs{maxDiscardRatio = 100} $ forAll genRelDomainTuple $ uncurry prop_firstRelElement

    putStrLn "\nChecks that the result of every relationship linked to all elements in the domain is in the domain"
    quickCheckWith stdArgs{maxDiscardRatio = 100} $ forAll genRelDomainTuple $ uncurry prop_secondRelElement