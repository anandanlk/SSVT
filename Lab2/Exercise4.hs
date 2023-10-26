module Exercise4 where
    
import Data.List
import LTS
import Test.QuickCheck
import Exercise2 hiding (main)

{-

---- EXERCISE 4 ---------------------------------------------------------------

    > Time Spent: 8 hours

    > Description:

        Problem:
            1. Implement the after function (infix) for IOLTS corresponding with the definition in the
            Tretmans paper. Use the following specification: after :: IOLTS -> Trace -> [State]
            2. Create tests to test the validity of your implementation with both traces and straces.

        Definition:
            Let q be a state in a transition system, and let Q be a set of states, then
                        x
            1. out(q)=def {x∈LU|q−→}∪{δ|δ(q)}
            2. out(Q) =def {out(q)|q∈Q}

        Solution:
            IOLTS = ([State], [Label], [Label], [LabeledTransition], State)
            1. Traverse from start state by applying each LabeledTransition and traverse to new state.
               The new state bcomes the state(s). Apply same rule to new states for all the Labeled Transitions.
            2. If Delta is Label then the next state remains same
            3. If Tau is Label then traverse to next state without any input.
-}



after :: IOLTS -> Trace -> [State]
(after) (states, input_labels, output_labels, transitions, start_state) traces =
    after' transitions traces start_state
    -- Sending transtions, traces and start start to after'function
    -- which needs to be called for all next states

-- if trace(s) is/are empty then return the current state
after' :: [LabeledTransition] -> Trace -> State -> [State]
after' transitions [] current_state = [current_state]

-- if not find nextStates and call after' function for all the next states
after' transitions (current_traces:remaining_traces) current_state =
    let
        -- Find next state(s)
        nxt_states = nextStates transitions current_traces current_state
    in 
    -- call after' function for all the next states
    concatMap (after' transitions remaining_traces) nxt_states
    where
    nextStates ::  [LabeledTransition] -> Label -> State -> [State]
    nextStates transitions trace state
        | trace == "delta" = [state]
        | trace == "tau" = [ns' | (ts,tl,ns')<- transitions , state==ts]
        | otherwise = [ns' | (ts,tl,ns')<- transitions , state==ts , trace==tl]

new = createIOLTS [(0, "tau", 0), (0, "?coin", 1), (0, "?button", 2), (1, "!cola", 2), (1, "!coin", 3), (3, "!beer", 4), (2, "!receipt", 4), (4, "tau", 4)]

main :: IO ()
main = do
    -- Traces
    -- putStrLn "coffeeModel1 = createIOLTS [(1, \"?coin\", 2), (2, \"!tea\", 3), (2, \"!coffee\", 4)]"
    -- putStrLn "coffeeModel1 `after` [\"coin\",\"coffee\"]"
    -- print (coffeeModel1 `after` ["coin","coffee"]) -- Answer should be [4]
    -- putStrLn "coffeeModel1 `after` [\"coin\"]"
    -- print (coffeeModel1 `after` ["coin"]) -- Answer should be [2]
    -- putStrLn "coffeeModel1 `after` [\"delta\"]"
    -- print (coffeeModel1 `after` ["delta"]) -- Answer should be [1]
    
    -- -- Straces
    -- putStrLn "coffeeModel1 `after` [\"tau\"]"
    -- print (coffeeModel1 `after` ["tau"]) -- Answer should be [2]
    -- putStrLn "coffeeModel4 = createIOLTS [(1, \"?button\", 2), (2, \"!tea\", 3), (1, \"?coin\", 4), (4, \"!coffee\", 5)]"
    -- putStrLn "coffeeModel4 'after' [\"tau\"]"
    -- print (coffeeModel4 `after` ["tau"]) -- Answer should be [2, 4]

    print(new `after` ["tau"])
    print(new `after` ["?coin", "tau"])
    print(new `after` ["?coin", "?coin", "tau"])
    print(new `after` ["?coin", "!beer"])
    print(new `after` ["tau", "?button", "!receipt", "tau"])