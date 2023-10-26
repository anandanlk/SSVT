module Exercise5 where
import Data.List

{-

---- EXERCISE 5 ---------------------------------------------------------------

    > Time Spent: 1.5 hours

    > Description:
    
        Use the datatype for relations from the previous exercise, plus

            infixr 5 @@
            (@@) :: Eq a => Rel a -> Rel a -> Rel a
            r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

        to define a function:

            trClos :: Ord a => Rel a -> Rel a

        that gives the transitive closure of a relation, represented as an ordered list of pairs. E.g.,
        trClos [(1,2),(2,3),(3,4)] should give [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)].

-}
type Rel a = [(a,a)]

-- Find relations to make two pairs transitive
infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- Apply @@ on union of the original set along with the new transitive pairs until no further changes observed
trClos :: Ord a => Rel a -> Rel a
trClos = fp (\x -> sort (x `union` (x @@ x)))

-- From Lecture6.hs
fp :: Eq a => (a -> a) -> a -> a 
fp f = until (\ x -> x == f x) f

    
main :: IO ()
main = do
    putStr "trClos [(1,2),(2,3),(3,4)]: "
    print (trClos [(1,2),(2,3),(3,4)])