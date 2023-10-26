module Exercise1 where

import Data.List
import LTS
import Test.QuickCheck

{-

---- EXERCISE 1 ---------------------------------------------------------------

    > Time Spent: 1h

    > Description:

    We need to write a function that validates an IOLTS:

        validateLTS :: IOLTS -> Bool

        The Tretmans paper and the slides have given us the following definition
    of an IOLTS:

        Definition 6. A labelled transition system with inputs and outputs is a 5-tuple
        <Q,LI , LU ,T,q0> where
         - Q,LI ∪ LU ,T,q0 is a labelled transition system in LTS(LI ∪ LU );
         - LI and LU are countable sets of input labels and output labels, respectively,
        which are disjoint: LI ∩ LU = ∅.

    And of course, we also need to know the definition of an LTS:

        Definition 1. A labelled transition system is a 4-tuple 	Q,L,T,q0
        where
         - Q is a countable, non-empty set of states;
         - L is a countable set of labels;
         - T ⊆ Q × (L ∪ {τ}) × Q, with τ /∈ L, is the transition relation;
         - q0 ∈ Q is the initial state.


        Therefore, our function should validate that all the mentioned properties hold.
    As we are in Haskell, incountable sets are something we don't need to worry about,
    as the implementation given for the IOLTS uses lists, which are always countable.

    > Testing:

        To test our implementation we have used some of the provided correct 
    IOLTS from the 'LTS.hs' file. We print the resulting value through the terminal.
    This allowed us to know that our implementation worked for correctly formed
    IOLTS.

        Then we included some counterexamples, for which we knew that validateLTS
    should not hold. These are called 'counter1' and 'counter2'.

-}

-- > Function   ---------------------------------------------------------------
validateLTS :: IOLTS -> Bool
validateLTS x =
    prop_start_in_states x &&
    prop_non_empty_states x &&
    prop_set_states x &&
    prop_set_input_labels x &&
    prop_set_output_labels x &&
    prop_disjoint_in_out x &&
    prop_transition_relation x

-- Start state must be in states list
prop_start_in_states :: IOLTS -> Bool
prop_start_in_states (states, _, _, _, q0) = q0 `elem` states

-- States must be a non empty list
prop_non_empty_states :: IOLTS -> Bool
prop_non_empty_states (states, _, _, _, _) = states /= []

-- States must be a set
prop_set_states :: IOLTS -> Bool
prop_set_states (states, _, _, _, _) = states == nub states

-- Input labels must be a set
prop_set_input_labels :: IOLTS -> Bool
prop_set_input_labels (_, input_labels, _, _, _) = input_labels == nub input_labels

-- Input labels must be a set
prop_set_output_labels :: IOLTS -> Bool
prop_set_output_labels (_, _, output_labels, _, _) = output_labels == nub output_labels

-- Input and output lists must be disjoint
prop_disjoint_in_out :: IOLTS -> Bool
prop_disjoint_in_out (_, input_labels, output_labels, _, _) = null(intersect input_labels output_labels)

-- Transition relation
-- For all transitions, all start states must be in states and all end states must be in states, and all labels must be in list or they must be taus
prop_transition_relation :: IOLTS -> Bool
prop_transition_relation (states, input_labels, output_labels, transitions, q0) = all (==True) (map (f (input_labels ++ output_labels ++ [tau]) states) transitions)
    where f labels states (start, label, end) = start `elem` states && end `elem` states && label `elem` labels


-- > Properties ---------------------------------------------------------------
propValid_start_in_states :: IOLTS -> Property
-- if validateLTS l holds then q0 `elem` s should hold
propValid_start_in_states l@(s,il,ol,t,q0) = (validateLTS l) ==> (q0 `elem` s)

propValid_start_not_in_states :: IOLTS -> Property
-- if q0 is not an s element then not validateLTS should hold
propValid_start_not_in_states l@(s,il,ol,t,q0) = (q0 `notElem` s) ==> (not (validateLTS l))

propValid_non_empty_set_states :: IOLTS -> Property
-- if validateLTS l holds then s needs to be a non empty set
propValid_non_empty_set_states l@(s,il,ol,t,q0) = (validateLTS l) ==> s /= [] && s == nub s

propValid_empty_states :: IOLTS -> Property
-- if list of states (s) is null ([]) then not validateLTS l should hold
propValid_empty_states l@(s,il,ol,t,q) = (null s) ==> (not (validateLTS l))

propValid_non_set_states :: IOLTS -> Property
-- if list of states (s) is not a set then not validateLTS l should hold
propValid_non_set_states l@(s,il,ol,t,q) = (s /= nub s) ==> (not (validateLTS l))

propValid_set_disjoint_labels :: IOLTS -> Property
-- if validateLTS l holds then input labels (il) and output labels (ol) must be disjoint sets
propValid_set_disjoint_labels l@(s,il,ol,t,q) = (validateLTS l) ==> il == nub il && ol == nub ol && null(intersect il ol)

propValid_non_set_input_labels :: IOLTS -> Property
-- if list of input labels (il) is not a set then not validateLTS l should hold
propValid_non_set_input_labels l@(s,il,ol,t,q) =  (il /= nub il) ==> (not (validateLTS l))

propValid_non_set_output_labels :: IOLTS -> Property
-- if list of output labels (il) is not a set then not validateLTS l should hold
propValid_non_set_output_labels l@(s,il,ol,t,q) =  (ol /= nub ol) ==> (not (validateLTS l))

propValid_transition :: IOLTS -> Property
-- if validateLTS l holds then for every element of labelled transitions (t) both states must be in the state
-- list (s) and the label must be in the union of input labels (il), output labels and {tau}.
propValid_transition l@(s,il,ol,t,q) = (validateLTS l) ==> (all (==True) (map (f (il ++ ol) s) t))
    where f l s (q1, label, q2) | label == "tau" = q1 `elem` s && q2 `elem` s
                                              | otherwise = q1 `elem` s && q2 `elem` s && label `elem` l

propValid_non_label_transition :: IOLTS -> Property
-- if one element of labelled transitions (t) cointains a label that is not in the union of input labels (il)
-- output labels (ol) or {tau} then not validateLTS should hold
propValid_non_label_transition l@(s,il,ol,t,q) = (elem False (map (f (il ++ ol ++ [tau]) s) t)) ==> (not (validateLTS l))
    where f l s (q1, label, q2) = label `elem` l

tmp :: IOLTS -> Bool
tmp l@(s,il,ol,t,q) = (elem False (map (f (il ++ ol ++ [tau]) s) t))
    where f l s (q1, label, q2) = label `elem` l

propValid_non_states_transition :: IOLTS -> Property
-- if one element of labelled transitions (t) cointains a state that is not in the states list (s) then 
-- not validateLTS should hold
propValid_non_states_transition l@(s,il,ol,t,q) = (elem False (map (f (il ++ ol ++ [tau]) s) t)) ==> (not (validateLTS l))
    where f l s (q1, label, q2) = q1 `elem` s && q2 `elem` s

-- > Main       ---------------------------------------------------------------
counter1 = ([],[tau],[tau],[],1)
counter2 = ([1],[tau],[tau],[(1,tau,1)],2)

main :: IO ()
main = do
    putStrLn ">>> Testing IOLTS:"
    print (validateLTS tretmanK1)

    putStrLn ">>> Testing IOLTS 2:"
    print (validateLTS tretmanR1)

    putStrLn ">>> Testing tau in IOLTS:"
    print (validateLTS coffeeModel6)

    putStrLn ">>> Testing counter1:"
    print $ validateLTS counter1

    putStrLn ">>> Testing counter2:"
    print $ validateLTS counter2
