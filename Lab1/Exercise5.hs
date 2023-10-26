module Exercise5 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import System.IO (hReady)

data Boy = Matthew | Peter | Jack | Arnold | Carl deriving (Eq,Show)

{-

---- EXERCISE 5 ---------------------------------------------------------------

    > Time Spent: 16h

    > Description:

        We are meant to write a program that solves the crime investigation
    that is presented to us. The first step is to translate the natural
    language into a logical one. For that, we need to create the following
    atoms:

         - M: Matthew is saying the truth   - /M: Matthew is lying.
         - P: Peter is saying the truth     - /P: Peter is lying.
         - J: Jack is saying the truth      - /J: Jack is lying.
         - A: Arnold is saying the truth    - /A: Arnold is lying.
         - C: Carl is saying the truth      - /C: Carl is lying.

        Now we need to translate the declarations of each of the kids.
    To do so, we need to imagine each of the declarations as a function
    that will be evaluated on a boy and will give us back a Bool value.
    For example, lets consider Matthew's declaration. The proper way
    of translating it into specifications would be the following:

        matthew x = x /= Carl && x /= Matthew

        Why? Because if we consider he is saying the truth, then neither
    him nor Carl would be guilty for the theft. As a function that 
    would mean that:

        matthew Matthew = False
        matthew Carl    = False
        matthew Jack    = True
        matthew Peter   = True
        matthew Arnold  = True

        We follow that process for each of the declarations to translate
    all of them into code to be able to later identify the thief. By doing
    this we get a declaration function for each of the kids (denoted by
    their names, all in lowercase). With them we compose the 'declarations'
    list (made of Boy -> Bool functions).

        We are now asked to code two functions 'accuses' and 'accusers'.
    Let's first explain 'accuses'. We already have our list of 'declarations',
    therefore, to implement 'accuses' we just need to use a guard according
    to the first input and then call the respective declaration function
    upon the second input. For example, if we consider the call

        >>> accuses Matthew Peter

        We would basically have to use the 'matthew' declaration function 
    and return its Bool value result.

        To implement 'accusers' we can just use the 'accuses' function we
    just implemented. Why? Let's see this through an example. We wan't to get
    a list of all the kids that accuse Matthew. Therefore, we just need to get 
    all the 'x' values such that

        >>> accuses x Matthew == True

        We can do that easily by filtering the list 'boys' with the accuses 
    function.

        Finally, we arrive to the implementation of 'guilty' and 'honest'.
    We have created a separate function 'liars' that identifies all the kids
    who have not told the truth. This will return a list with the names of the 
    two liars, and 'honest' will be its complementary list.

        To implement 'guilty' we have followed the following thought process.
    We know for sure that 2 kids are lying and 3 are telling the truth. Therefore,
    the Bool value of their declarations should also be 2 Falses and 3 Trues.
    With this in mind, we just need to get the name 'x' (if it exists) of the kid
    that verifies that the result of evaluating the 'declarations' list on 'x'
    returns a list with 3 Falses and 2 Trues.

        Once we have our thief, its really easy to identify the liars and the 
    honests. We just evaluate again the 'declarations' list on the thief's name
    and using zip we identify the name of our liars. Then, as previously stated,
    the honest list is the complementary of the liars list.
-}

-- > Functions          -------------------------------------------------------
boys :: [Boy]
boys = [Matthew, Peter, Jack, Arnold, Carl]

matthew, peter, jack, arnold, carl :: Boy -> Bool
matthew x = x /= Carl && x /= Matthew
peter   x = x == Matthew || x == Jack
jack    x = not (matthew x) && not (peter x)
arnold  x = matthew x /= peter x
carl    x = not (arnold x)

declarations :: [Boy -> Bool]
declarations = [matthew, peter, jack, arnold, carl]

accuses :: Boy -> Boy -> Bool
accuses x y | x == Matthew = matthew y
            | x == Peter = peter y
            | x == Jack = jack y
            | x == Arnold = arnold y
            | otherwise = carl y

accusers :: Boy -> [Boy]
accusers x = filter (`accuses` x) boys

guilty, liar, honest :: [Boy]
guilty = [ x | x <- boys,
                   length (filter (== False) (map (\f -> f x) declarations)) == 2]
liar   = [ x | (x,y) <- zip boys (map (\f -> f (head guilty)) declarations),
                not y]         
honest = boys \\ liar

-- > Main               -------------------------------------------------------
main :: IO()
main = do
    putStrLn ">>> Guilty:"
    print guilty

    putStrLn "\n>>> Liars:"
    print liar

    putStrLn "\n>>> Honest:"
    print honest


