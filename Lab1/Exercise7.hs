import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

{-

---- EXERCISE 7 ---------------------------------------------------------------

    > Time Spent: 6h

    > Description:

    We were asked to verify the implementation of the sub function and
    to write a recursive funciton nsub to compute the exact number of sub formulae

    > Testing:

    We wrote some quickcheck properties to test the sub function. We also wrote
    a quickcheck property to test the nsub function.

    For the sub function we test each possible case of the function. For the nsub
    function we test that the number of sub formulae is equal to the length of the
    set of sub formulae.

-}


-- > Given Code     -----------------------------------------------------------
type Name = Int

data Form = Prop Name | Neg  Form | Cnj [Form] | Dsj [Form] | Impl Form Form | Equiv Form Form deriving (Eq,Ord, Show)

sub :: Form -> Set Form
sub (Prop x) = Set [Prop x]
sub (Neg f) = unionSet (Set [Neg f]) (sub f)
sub f@(Cnj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Dsj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Impl f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Equiv f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)

-- > Functions       ----------------------------------------------------------

nsub :: Form -> Int
nsub (Prop x) = 1
nsub (Neg f) = 1 + nsub f
nsub (Cnj [f1,f2]) = 1 + nsub f1 + nsub f2
nsub (Dsj [f1,f2]) = 1 + nsub f1 + nsub f2
nsub (Impl f1 f2) = 1 + nsub f1 + nsub f2
nsub (Equiv f1 f2) = 1 + nsub f1 + nsub f2

-- > Properties   ---------------------------------------------------------------
prop_sub_1 :: Bool
prop_sub_1 = sub (Prop 1) == Set [Prop 1]

prop_sub_2 :: Bool
prop_sub_2 = sub (Neg (Prop 1)) == Set [Prop 1, Neg (Prop 1)]

prop_sub_3 :: Bool
prop_sub_3 = sub (Cnj [Prop 1, Prop 2]) == Set [Prop 1, Prop 2, Cnj [Prop 1, Prop 2]]

prop_sub_4 :: Bool
prop_sub_4 = sub (Dsj [Prop 1, Prop 2]) == Set [Prop 1, Prop 2, Dsj [Prop 1, Prop 2]]

prop_sub_5 :: Bool
prop_sub_5 = sub (Impl (Prop 1) (Prop 2)) == Set [Prop 1, Prop 2, Impl (Prop 1) (Prop 2)]

prop_sub_6 :: Bool
prop_sub_6 = sub (Equiv (Prop 1) (Prop 2)) == Set [Prop 1, Prop 2, Equiv (Prop 1) (Prop 2)]

prop_nsub_1 :: Form -> Bool
prop_nsub_1 f = nsub f == length' (sub f)

main :: IO ()
main = do
    putStrLn "\n>>> Testing prop_sub_1:"
    print prop_sub_1

    putStrLn "\n>>> Testing prop_sub_2:"
    print prop_sub_2

    putStrLn "\n>>> Testing prop_sub_3:"
    print prop_sub_3

    putStrLn "\n>>> Testing prop_sub_4:"
    print prop_sub_4

    putStrLn "\n>>> Testing prop_sub_5:"
    print prop_sub_5

    putStrLn "\n>>> Testing prop_sub_6:"
    print prop_sub_6

    putStrLn "\n>>> Testing prop_nsub_1:"
    quickCheck prop_nsub_1

instance Arbitrary Form where
    arbitrary = frequency 
        [ (10, Prop <$> arbitrary)
        , (1, Neg <$> arbitrary)
        , (1, Cnj <$> children)
        , (1, Dsj <$> children)
        , (1, Impl <$> arbitrary <*> arbitrary)
        , (1, Equiv <$> arbitrary <*> arbitrary)
        ]
        where children = sized $ \n -> vectorOf 2 arbitrary
