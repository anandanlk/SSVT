module Exercise7 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd
import Exercise3 hiding (main)
import Exercise5 hiding (main)

{-

---- EXERCISE 7 --------------------------------------------------------------

    > Time Spent: 20 mins

    > Description:

    We need to study if the composition of symmetric closure and transitive
    closure is conmutative. To do that, we initially tested on a simple example
    called 'test'. Through this example we illustrate that the use
    is not conmutative, as the result 'symClos -> trClos' is not the
    same as the one produced by 'trClos -> symClos'. 

    [(0,0),(0,1)] -> (sym) -> [(0,0),(0,1),(1,0)] ->
                  -> (tr)  -> [(0,0),(0,1),(1,0),(1,1)]

    [(0,0),(0,1)] -> (tr)  -> [(0,0),(0,1)] ->
                  -> (sym) -> [(0,0),(0,1),(1,0)]
-}

test = [(0,0),(0,1)]

main :: IO()
main = do
    putStrLn ">>> -> symClos -> trClos:"
    print $ trClos(symClos test)
    putStrLn ">>> -> trClos -> symClos:" 
    print $ symClos(trClos test)