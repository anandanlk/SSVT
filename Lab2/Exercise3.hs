module Exercise3 where

import Data.List
import LTS
import Test.QuickCheck
import Exercise2 hiding (main)

{-

---- EXERCISE 3 ---------------------------------------------------------------

    > Time Spent: 5h

    > Description:

        In this exercise we are meant to implement the straces of a given IOLTS.
    To do that, we have first studied what straces are and how are they build by
    reading Tretmans paper. We noticed in our paper examples what seemed as a simple
    but working way of implementing this. The steps to follow are the following:

         1. We need to get all of the states from the states list (s) who do not
            have a transition (q1,l,q2) where they are the first state (q1)
            with an output label (l). These are the states that allow for quiescence.

         2. Once we have our filtered quiescent states, we need to create the
            quiescent transitions for everyone of them. These transitions are of the
            form (q,delta,q).

         3. Now, thanks to our TAs, we already have a working and correct implementation 
            of the traces function for LTS. Therefore, we can just call this function by
            joining our input labels, output labels and delta as our labels (l) and by
            joining our transitions and quiescent transitions as our new list of 
            transitions (t).

        The first step is the most complex one, and for it we have created several
    functions with very specific functionality. These are commented on the code itself.

    > Testing:

        We were unsure on how to test the code. Our first concer was that the straces 
    function returns an infinite list, which makes everything harder to test (at least
    from our inexperienced point of view). For that reason, the only property we were
    able to think of was monotony. Straces verify monotony, meaning that for every trace
    that appears in the list, there is a bigger list that cointains it as its prefix 
    (in case of finite list, this does not apply for the last element). For example, lets
    consider this list of traces:
    
        [[],["delta"],["delta","delta"]]

        As we can see, the second element (["delta"]) is the prefix of the third element
    ["delta","delta"]
-}

-- > Functions  ---------------------------------------------------------------
qState_check_transition_output :: [Label] -> LabeledTransition -> Bool
-- Check if a labeled transition contains an output label
qState_check_transition_output ol (q1,l,q2) = l `elem` ol

qState_check_transition_state1 :: State -> LabeledTransition -> Bool
-- Check if a labeled transition contains an state as its first state
qState_check_transition_state1 q (q1,l,q2) = q1 == q

qState_transitions_state1 :: IOLTS -> State -> [LabeledTransition]
-- Get a list of all the transitions from an IOLTS where a given state occupies the first
-- state position
qState_transitions_state1 (s,il,ol,t,q0) q = filter (qState_check_transition_state1 q) t

qState_transitions_state1_output :: IOLTS -> State -> [LabeledTransition]
-- Get a list of all the transitions from an IOLTS where a given state occupies the first
-- state position and that have a label from the output labels list
qState_transitions_state1_output l@(s,il,ol,t,q0) q = filter (qState_check_transition_output ol) (qState_transitions_state1 l q)

qStates :: IOLTS -> [State]
-- Get a list of all the state who do not have any transitions where they occupy the first
-- state positon and a label from the output labels list
qStates l@(s,il,ol,t,q0) = filter (null . qState_transitions_state1_output l) s

qTransitions :: [State] -> [LabeledTransition]
-- Generate the quiescent transitions for a given list of states
qTransitions q = [(x,delta,x) | x <- q]

straces :: IOLTS -> [Trace]
straces l@(q,il,ol,t,q0) = traces (q,il ++ ol ++ [delta],t ++ qTransitions(qStates l), q0)

-- > Generator  ---------------------------------------------------------------
genInt :: Gen Int
-- Generate an arbitrary postive number to be used as the limit
genInt = arbitrary `suchThat` (>0)

genTraces :: Gen [Trace]
-- Generate a finite amount of straces for a randomly generated IOLTS
genTraces = do
    iolts <- genIOLTS
    limit <- genInt
    return $ take limit (straces iolts)

-- > Properties ---------------------------------------------------------------

prop_monotony :: [Trace] -> Bool
-- The straces are monotonous, meaning that for every trace there should be
-- another bigger trace later on the list that contains it.
prop_monotony t = all (\t1 -> all (\t2 -> not(t1 `isPrefixOf` t2) || elem t1 (takeWhile (/= t2) t) || t1 == t2) t) t

-- > Main   -------------------------------------------------------------------
main :: IO()
main = do
    putStrLn ">>> Testing Monotony: "
    quickCheck $ forAll genTraces prop_monotony
