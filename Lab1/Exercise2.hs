module Exercise2 where

import System.Random
import Test.QuickCheck

{-

---- EXERCISE 2 ---------------------------------------------------------------

    > Time Spent: 3h

    > Description:

        The approach that we took for this exercise was the following. First, 
    we dediced to get an initial working implementation of subsequences. We 
    include that in the following block code:

        subsequences' :: [a] -> [[a]]
        subsequences' [] = [[]]
        subsequences' (x:xs) = subsequences' xs ++ map (x :) (subsequences' xs)

        However, as with the factorial, we wanted to try and create a tail
    recursive version of the function. We followed the same idea used in
    the factorial function of Exercise1 to come up with the 'subseq' function,
    which uses tail recursion. We wanted to do this because its a good way
    of getting familiarized with tail recursion and because it has considerable 
    computational benefits (no stack overflow, for example).

    > Questions:

    1.  Is the property hard to test? If you find that it is, can you given a 
    reason why?

        Yes, it is indeed hard to test. The main reason for that is the size
    of the lists gets eventually to big to compute. How did we notice this?
    At first we were using a list generator that created lists of arbitrary
    lenght, which we named 'genInfAList'. After trying to run a quickCheck test, 
    the computer eventually killed our terminal process. We then recalled that
    during Workshop 1 we proved that if |A| = n, then |P(A)| = 2^n. Therefore,
    for a list A such that |A| = 100, we would have that |P(A)| = 2^10 = 1024
    elements. And it is also worth noting that each of those elements are lists
    that go from 0 elements to 100. Therefore, the computational cost of such
    a calculation exceeds our PC capabilities, resulting in difficulties to 
    test the function.

        How did we try and overcome this problem? We created a second generator
    called 'genBoundAList', which generates list up to a certain lenght. As we also
    wanted to have random lenght lists, we created a second generator ('genMaxSize') 
    which provides us with a random Int bigger or equal than 0 and smaller or equal than
    18. The upper bound (18) was chosen by trying different values and seeing wether
    VS Code would kill the process or not. Depending on the computer, the value 
    fluctuated from 17 to 20, so we chose a rather safe upper bound. We also decided to 
    use Int rather than Integer for two reasons:
     1. The function 'vectorOf' takes an Int, not an Integer, and we use it to generate 
        the limited length lists.
     2. We are using bounded values smaller or equal than 18, so there is no actual
        difference between Int and Integer within this range

    **DISCLAIMER**: We also noticed after finishing the previously explained process 
    that we were meant to be testing on much simpler lists of the form [1..n]. To avoid 
    any problems with the correction we have two separate mains:
     - main     It has the expected property test, using lists of the form [1..n]
     - main'    It is our own main, where we use our custom generators that allow
                us to generate lists of arbitrary types. In this case, we test the
                function using both Integer list and String lists. It is worth noting
                that this list are totally random, meaning that they will probably
                not be of the form [1..n], but rather something similar to 
                [1,93,24,45,...].

    And we also have two separate properties:
     - prop     The expected one, which takes a random integer from quickCheck to 
                use [1..n] lists.
     - prop'    Our property, which takes a randomly generated list using our
                custom generators.

    2.  When you perform the test for exercise 4, what are you testing actually?
    Are you checking a mathematical fact? Or are you testing whether
    subsequences satisfies a part of its specification? Or are you testing
    something else still?

        We belive we are only testing that subsequences satisfies a part of its 
    specification. Why? Cause we are only checking wether the lenght of the
    generated subsequences is equals to the expected mathematical value, but
    we are not really checking if the list of subsequences is correct. What we
    mean by this is that our list could have for example duplicates and we wont
    be noticing as the lenght of the total list is still the expected mathematical
    value.

        We are not checking or proving a mathematical fact because that would need
    to be done using induction, just as we did in Workshop 1.
        
    > Testing:

        As we have already explain our properties during Question 1, we will not
    include a lot more new information in this part.

        However, it is worth noting that from this test we can only be sure
    that our function works for list of length in {1...18}. Why? Because they
    are the only values that make our computer not to crash. We cannot assure
    our code works for bigger values.

-}

-- > Function   ---------------------------------------------------------------
subsequences :: [a] -> [[a]]
subsequences l = subsequencesAux l [[]]

subsequencesAux :: [a] -> [[a]] -> [[a]]
subsequencesAux [] r = r
subsequencesAux l  r = subsequencesAux (tail l) (r ++ map (head l :) r)

-- > Generators ---------------------------------------------------------------
genInfAList :: Arbitrary a => Gen [a]
genInfAList = arbitrary

genBoundedAList :: Arbitrary a => Gen [a]
genBoundedAList = do
    maxSize <- genMaxSize
    vectorOf maxSize arbitrary

genMaxSize :: Gen Int
genMaxSize = arbitrary `suchThat` (\x -> x >= 0 && x <= 18)

-- > Property   ---------------------------------------------------------------
prop' :: [a] -> Bool
prop' l = length (subsequences l) == 2^length l

prop :: Int -> Bool
prop n = length (subsequences [1..n]) == 2^n

-- > Mains      ---------------------------------------------------------------
main' :: IO()
main' = do
    putStrLn ">>> Testing Integers:"
    quickCheck $ forAll (genBoundedAList :: Gen[Integer]) prop'

    putStrLn "\n>>> Testing Strings:"
    quickCheck $ forAll (genBoundedAList :: Gen[String]) prop'

main :: IO()
main = do 
    putStrLn ">>> Testing:"
    quickCheck $ forAll genMaxSize prop
