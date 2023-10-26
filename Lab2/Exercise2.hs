module Exercise2 where

import Data.List
import LTS
import Test.QuickCheck
import Exercise1 hiding (main)

{-

---- EXERCISE 2 ---------------------------------------------------------------

    > Time Spent: 2h

    > Description:

        In this exercise we are meant to implement a generator for IOLTS. For
    that purpose, we have first programmed what we belive is a correct
    generator for IOLTS. We have modularized the implementation of this generator
    in smaller generators to make the code easier to read and understand. The
    basic idea followed was to first get the states list, then the labels (both
    input and output) and then, using the previous create transitions by using
    random elements of the states lists and the union of input and output lists.
    We would like to highlight the use of 'elements' function, which generates
    one of the given values. We used this to pick a random element of the 
    previously generated lists (states, input_labels and output_labels).

        Once the correct generator was build, we build other generators that
    could comply with the properties of exercise 1. Basically, we tweaked
    the generator depending on what property we wanted to achieve. For example,
    if we want to test a property were states need to be an empty list
    we would have created a generator that always returns IOLTS with an 
    empty state list (which ofc are non-valid IOLTS). We would like to 
    clarify that the only generator which returns valid IOLTS is 'genIOLTS',
    the rest of them just return IOLTS with certain characteristics to test
    Exercise1 properties.

    > Testing:

        We would like the importance of QuickCheck. We actually spent much more
    time fixing our generators (which we believed to be correct) rather than 
    programming them. Thanks to QuickCheck we realised we were not taking into
    consideration certain properties. For example, at first, we were not taking
    into account that both input_labels and output_labels need to be disjoint and
    QuickCheck made us realise that something was wrong as some tests were getting
    skipped!
-}

-- > Partial Generators  ------------------------------------------------------
genState :: Gen State
-- Generator for states (it is just a random state generator)
genState = arbitrary

genStateNotInStates :: [State] -> Gen State
-- Generator for states that is not in the given states list
genStateNotInStates s = arbitrary `suchThat` (`notElem` s)

genLabel :: [Label] -> Gen Label
-- Generator for labels (it is just a random string generator)
genLabel l = arbitrary `suchThat` (`notElem` l)

genLabelledTransition :: [State] -> [Label] -> Gen LabeledTransition
-- Generator for a labelled transtion given the states and labels
genLabelledTransition states labels = do
    q1 <- elements states
    label <- elements labels
    q2 <- elements states
    return (q1, label, q2)

genWrongLabelLabelledTransition :: [State] -> [Label] -> Gen LabeledTransition
-- Generator for a labelled transtion whose label is not in the labels list
genWrongLabelLabelledTransition states labels = do
    q1 <- elements states
    label <- genLabel labels
    q2 <- elements states
    return (q1, label, q2)

genWrongState1LabelledTransition :: [State] -> [Label] -> Gen LabeledTransition
-- Generator for a labelled transtion whose state1 is not in the states list
genWrongState1LabelledTransition states labels = do
    q1 <- genStateNotInStates states
    label <- elements labels
    q2 <- elements states
    return (q1, label, q2)

genWrongState2LabelledTransition :: [State] -> [Label] -> Gen LabeledTransition
-- Generator for a labelled transtion whose state2 is not in the states list
genWrongState2LabelledTransition states labels = do
    q1 <- elements states
    label <- elements labels
    q2 <- genStateNotInStates states
    return (q1, label, q2)

-- > Generators ---------------------------------------------------------------
genIOLTS :: Gen IOLTS
-- General perfect generator
genIOLTS = do
    states <- listOf1 genState
    input_labels <- listOf $ genLabel []
    output_labels <- listOf $ genLabel input_labels
    transitions <- listOf $ genLabelledTransition states (nub (input_labels ++ output_labels ++ [tau]))
    q0 <- elements states
    return (nub states, nub input_labels, nub output_labels, transitions,q0)

genIOLTSStartStateNotInStates :: Gen IOLTS
-- Generator that creates IOLTS with an start state that is not in states list
genIOLTSStartStateNotInStates = do
    states <- listOf1 genState
    input_labels <- listOf $ genLabel []
    output_labels <- listOf $ genLabel input_labels
    transitions <- listOf $ genLabelledTransition states (nub (input_labels ++ output_labels ++ [tau]))
    q0 <- genStateNotInStates states
    return (nub states, nub input_labels, nub output_labels, transitions,q0)

genIOLTSEmptyStates :: Gen IOLTS
-- Generator that creates IOLTS with empty states list
genIOLTSEmptyStates = do
    input_labels <- listOf $ genLabel []
    output_labels <- listOf $ genLabel input_labels
    transitions <- listOf $ genLabelledTransition [] (nub (input_labels ++ output_labels ++ [tau]))
    q0 <- genState
    return ([], nub input_labels, nub output_labels, transitions,q0)

genIOLTSTransitionLabelNotInLabels :: Gen IOLTS
-- Generator that creates IOLTS with a transition that cointains a label that is
-- not in the union of the input labels, output labels and {tau}.
genIOLTSTransitionLabelNotInLabels = do
    states <- listOf1 genState
    input_labels <- listOf $ genLabel []
    output_labels <- listOf $ genLabel input_labels
    transitions <- listOf $ genLabelledTransition states (nub (input_labels ++ output_labels ++ [tau]))
    wrong_transitions <- listOf1 $ genWrongLabelLabelledTransition states (nub (input_labels ++ output_labels ++ [tau]))
    q0 <- elements states
    return (nub states, nub input_labels, nub output_labels, transitions ++ wrong_transitions ,q0)

genIOLTSTransitionStateNotInStates :: Gen IOLTS
-- Generator that creates IOLTS with a transition that cointains a state that is
-- not in the states list
genIOLTSTransitionStateNotInStates = do
    states <- listOf1 genState
    input_labels <- listOf $ genLabel []
    output_labels <- listOf $ genLabel input_labels
    transitions <- listOf $ genLabelledTransition states (nub (input_labels ++ output_labels ++ [tau]))
    wrong1_transitions <- listOf1 $ genWrongState1LabelledTransition states (nub (input_labels ++ output_labels ++ [tau]))
    wrong2_transitions <- listOf1 $ genWrongState2LabelledTransition states (nub (input_labels ++ output_labels ++ [tau]))
    q0 <- elements states
    return (nub states, nub input_labels, nub output_labels, transitions ++ wrong1_transitions ++ wrong2_transitions,q0)

-- > Main   -------------------------------------------------------------------
main :: IO()
main = do
    putStrLn ">>> Testing 'propValid_start_in_states':"
    quickCheck $ forAll genIOLTS propValid_start_in_states
    putStrLn ">>> Testing 'propValid_start_not_in_states':"
    quickCheck $ forAll genIOLTSStartStateNotInStates propValid_start_not_in_states
    putStrLn ">>> Testing 'propValid_non_empty_set_states':"
    quickCheck $ forAll genIOLTS propValid_non_empty_set_states
    putStrLn ">>> Testing 'propValid_empty_states':"
    quickCheck $ forAll genIOLTSEmptyStates propValid_empty_states
    -- putStrLn ">>> Testing 'propValid_non_set_states':"
    -- quickCheck $ forAll genIOLTSNonSetStates propValid_non_set_states
    putStrLn ">>> Testing 'propValid_set_disjoint_labels':"
    quickCheck $ forAll genIOLTS propValid_set_disjoint_labels
    -- putStrLn ">>> Testing 'propValid_non_set_input_labels':"
    -- quickCheck $ forAll genIOLTSNonSetInputLabels propValid_non_set_input_labels
    -- putStrLn ">>> Testing 'propValid_non_set_output_labels':"
    -- quickCheck $ forAll genIOLTSNonSetOutputLabels propValid_non_set_output_labels
    putStrLn ">>> Testing 'propValid_transtion':"
    quickCheck $ forAll genIOLTS propValid_transition
    putStrLn ">>> Testing 'propValid_non_label_transition':"
    quickCheck $ forAll genIOLTSTransitionLabelNotInLabels propValid_non_label_transition
    putStrLn ">>> Testing 'propValid_non_states_transtion':"
    quickCheck $ forAll genIOLTSTransitionStateNotInStates propValid_non_states_transition
