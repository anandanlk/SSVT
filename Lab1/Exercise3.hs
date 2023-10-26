module Exercise3 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

{-

---- EXERCISE 3 ---------------------------------------------------------------

    > Time Spent: 4h

    > Description:

        We are asked to implement all the predicates corresponding to exercise 3
    from Workshop 2. This predicates are the following:

         - Predicate 1:     even
         - Predicate 2:     (\x -> even x && x > 3) 
         - Predicate 3:     (\ x -> even x || x > 3)
         - Predicate 4:     (\ x -> (even x && x > 3) || even x)

        Our objective is to print a descending strength list of all the implemented
    properties, that is, position 1 should be occupied by the strongest predicate
    and position 4 by the weaker predicate.

        To achieve our objective we decided to work with (Int -> Bool, String) tuples.
    By doing this, we could order our list of tuples according to the first element
    of each tuple by comparing their strength with the rest of the items.

        To do that we discovered the function 'sortBy', whose header is included in
    the following line:

        sortBy :: (a -> a -> Ordering) -> [a] -> [a]

        As we can see, it takes a compare function that returns an Ordering and a list
    of items and it returns the ordered list according to that given comparing function.
    How do we now that function is a compare function? Well, we found an example on 
    Haskell's documentation. With this in mind, we knew we needed to program our own
    compare function, which would take to predicates and decide wether they are equal (EQ),
    one is stronger than the other (GT) or one is weaker than the other (LT). The header
    of a compare function should be as the following one:

        compare :: a -> a -> Ordering

        In our case a will be (Int -> Bool, String) and as we already have the function 
    stronger and weaker, we finalized our compare function.

        Finally, we were only missing a way to print all the lists. This could be easily
    done using a map function, were we take our list of tuples and just return a list
    of the second value of each tuple (which is a String, therefore Showable). 

    > Predicate Order:

        Here we justify the order of the predicates. We will use this justification
    to generate a manual test, were we just test that the ordered list is equal to 
    the one provided here. We will give the ordered list of predicates and then
    justify the reasons behind it.

        1.   Predicate 2:    (\x -> even x && x > 3) 
        2/3. Predicate 1:    even
        3/2. Predicate 3:    (\ x -> (even x && x > 3) || even x)
        4.   Predicate 4:    (\ x -> even x || x > 3)

        To justify the ordering we believe looking at the accepted sets of each
    of the predicates suffices.

        Predicate 2:    [4,6,8,10,12...]    --> Even numbers bigger or equal than 4
        Predicate 1:    [2,4,6,8,10...]     --> Even numbers
        Predicate 3:    [2,4,6,8,10...]     --> Even numbers, the second clause is
                                                weaker than the first one so even
                                                thought x = 2 is false for the first
                                                clause, it is true for the second one 
        Predicate 4:    [2,4,5,6,7,8...]    --> {2} U {x | x >= 4, x Natural}
                                                Same case as in the previous example,
                                                in this case with odd numbers bigger
                                                or equal than 5.

        It is easy to observe that:
        
        [4,6,8,10,12...] c [2,4,6,8,10...] c [2,4,5,6,7,8...]

        where 'c' denotes contained by. Therefore, Predicate 2 is the strongest as it
    has the smallest domain and Predicate 4 is the weakest one as it has the biggest 
    one.

    > Testing:

        To test our function, we just checked that the obtained order was the 
    same as the one indicated in the previous part. We did this by printing
    the sorted list of predicates through terminal.

-}

-- > Given Code     -----------------------------------------------------------
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker xs p q = stronger xs q p

-- > Predicates     -----------------------------------------------------------
pred1, pred2, pred3, pred4 :: Int -> Bool
pred1 = even
pred2 n = even n && n > 3
pred3 n = even n || n > 3
pred4 n = (even n && n > 3) || even n

predicates :: [(Int -> Bool, String)]
predicates = [(pred1,"pred1"), (pred2,"pred2"), (pred3,"pred3"), (pred4,"pred4")]

-- > Function       -----------------------------------------------------------
predicatesSorted :: [(Int -> Bool, String)]
predicatesSorted = sortBy (flip comparePredicates) predicates

comparePredicates :: (Int -> Bool, String) -> (Int -> Bool, String) -> Ordering
comparePredicates p q | stronger [(-10)..10] (fst p) (fst q) && weaker [(-10)..10] (fst p) (fst q) = EQ
                      | stronger [(-10)..10] (fst p) (fst q) = GT
                      | otherwise = LT

printPredicates :: [(Int -> Bool, String)] -> String
printPredicates ps = intercalate "\n" (map snd ps)

-- > Main       ---------------------------------------------------------------
main :: IO()
main = do 
    putStrLn ">>> Unsorted Predicates:"
    putStrLn $ printPredicates predicates

    putStrLn "\n>>> Sorted Predicates:"
    putStrLn $ printPredicates predicatesSorted