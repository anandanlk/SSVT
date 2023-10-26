module Exercise5 where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-------------------------------------------------------------------------------
-- Especification
-------------------------------------------------------------------------------

-- Rot13 is a single-letter substitution cipher that is, for example,
-- used in online forums for hiding spoilers. They way it works is really 
-- simple, rotating each letter from the alphabet with the one that is 13
-- positions ahead. This may seem a bit confusing at first, for that reason 
-- lets take a look at the following example of a simplified version of Rot13
-- which we named Rot1:
--
-- >>> rot1Char 'a'
-- >>> 'b'
--
-- It's pretty obvious what we are doing here. The function rot1Char just rotates
-- each letter with the letter that is right next to it on the alphabet. A question
-- may rise now: what happens to the letter 'z'? Well, as there is no letter after
-- 'z' in our alphabet, we basically start again from the beginning. This means that:
--
-- >>> rot1Char 'z'
-- >>> 'a'
--
-- To finish the explanation of rot1Char, consider the following scheme on how each letter
-- changes after applying the function:
--
-- abcdefghijklmnopqrstuvwxyz 
--              ↓
-- bcdefghijklmnopqrstuvwxyza
--
-- Now that we understand how Rot1 works for single characters, let's see an example on
-- how would it work for a whole word.
--
-- >>> rot1 "hello"
-- >>> "ifmmp"
--
-- We can see that rot1 function is just applying rot1Char to each of the characters
-- that compose the word "hello". Now that we understand how Rot1 works, it will be
-- much easier to understand how Rot13 works. Again, we will start understanding how 
-- rot13Char works and then we will move on to rot13. Consider the following example:
--
-- >>> rot13Char 'a'
-- >>> 'n'
--
-- What is happening in here? There is no need to be scared. Same way rot1Char rotated
-- each alphabet letter with the following one, rot13Char just rotates each letter with
-- the one that is 13 places forward in the alphabet. We can do it manually to clarify
--
-- a [b c d e f g h i j k l m] n -> We choose the 13th forward letter
--      We skip 12 letters
--
-- In this case, if we consider character 'z' we would simply obtain the following result:
--
-- >>> rot13Char 'z'
-- >>> 'm'
--
-- Let's also do this manually:
-- a b c d e f g h i j k l m n o p q r s t u v x y z [a b c d e f g h i j k l] m -> We choose the 13th forward letter
--                                                      We skip 12 letters
--
-- We can also see that we get 'm' back, which makes sense as 'm' is just before 'n'
-- in the alphabet, just as 'a' would follow next after 'z'. Now we can move on into a 
-- word example:
--
-- >>> rot13 "hello"
-- >>> "uryyb"
--
-- And again, we are just using rot13Char on each of the characters that compose the 
-- word "hello".
--
-- One important thing to notice is that if we rot13 a word twice, we get the same
-- word back. Why? Well, we are basically moving each letter 13 places forward and 
-- concating two rot13s means moving them 26 places, which is of course, not moving
-- at all.
--
-- a [b c d e f g h i j k l m n o p q r s t u v x y z] a

-------------------------------------------------------------------------------
-- Implementation
-------------------------------------------------------------------------------

-- We need to implement the previously specified cipher. To do that, we are going to 
-- need to make use of the 'mod' operator, which allows us to obtain the remainder of
-- a division of two numbers. What number should we use as a divisor? We basically
-- just need to use the number of letters in the alphabet, 26. Why do we need to do this? 
-- If we wrote all letters of the alphabet in a list, 'a' would be position 0, 
-- 'b' position 1, ... , 'z' position 25. We already know 'a' goes to 'n', which basically
-- means we go from position 0 to position 13. However, we need 'z' to go to 'm', that is,
-- from position 25 to position 12. Here is were 'mod 26' comes to the rescue, as we can 
-- just add 25+13 (38) and get the remainder of the division by 26 (38/26 = 1, remainder = 12) 
-- which will give us the expected 12 position that we search for. 
-- 
-- The 'mod 26' operator basically allows us to consider the alphabet as a circle rather 
-- than a line with two extremes values, that means we can consider 'a' as the letter
-- that comes right after 'z'.
--
-- To implement rot13 we first decided to implement a Char version, and then use this
-- together with a map to generate a [Char] version. In case of the rot13Char, we used
-- 'ord' function, which receives a char and returns its ASCII value, and 'chr' function,
-- which receives an ASCII value and returns the character assigned to it.

rot13Char :: Char -> Char
rot13Char c
    | isLower c = chr $ ((ord c - ord 'a') + 13) `mod` 26 + ord 'a'
    | isUpper c = chr $ ((ord c - ord 'A') + 13) `mod` 26 + ord 'A'
    | otherwise = c

rot13 :: [Char] -> [Char]
rot13 = map rot13Char

-------------------------------------------------------------------------------
-- Properties
-------------------------------------------------------------------------------

-- Property 1: rot13(rot13 cs) = cs
--             Example: rot13(rot13 "hello") = rot13("uryyb") = "hello"
--             Problem: whenever we try to convert strings that are like "\123"
--                      it fails miserably, because it interprets the number as 
--                      single character. 

propExercise5a :: [Char] -> Bool
propExercise5a cs = cs == rot13(rot13 cs)

main :: IO()
main = return()