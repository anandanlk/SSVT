module Exercise3 where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

data Shape = NoTriangle | Equilateral
    | Isosceles | Rectangular | Other deriving (Eq,Show)

{-

Time spent: 30 mins

Description:
Firts, we check whether the introduced triplet is a triangle or not. In case it is
we check if all sides are the same for the equilateral, if two sides are the same and one different
for the isosceles, if it verifies pythagoras theorem for rectangular and finally, if none
of those requirements are met, it means its just an irregular triangle

To check correctness we did some manual tests. We believe this suffices as a proof, as we have manually
tested through paper that those included values are in fact correct

-}

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z
    | x <= 0 || y <= 0 || z <= 0 || x + y <= z || x + z <= y || y + z <= x = NoTriangle
    | x == y && x == z = Equilateral
    | x == y && x /= z || x == z && y /= z || y == z && y /= x = Isosceles
    | x^2 + y^2 == z^2 || x^2 + z^2 == y^2 || y^2 + z^2 == y^2 = Rectangular
    | otherwise = Other



testRunner :: [Shape]
testRunner = map f [
    (-1,0,0), -- No Triangle
    (1,2,3), -- No triangle
    (1,1,1), -- Equilateral
    (2,2,1), -- Isosceles
    (3,4,5), -- Rectangular
    (2,3,4)
    ]
    where f (x,y,z) = triangle x y z

main :: IO()
main = return()