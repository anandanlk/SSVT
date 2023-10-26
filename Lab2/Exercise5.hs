module Exercise5 where

import Data.List
import LTS
import Test.QuickCheck

{-

---- EXERCISE 5 ---------------------------------------------------------------

    > Time Spent: 8h

    > Description:

        We are to meant to test our own implementation of the smartdoor against other
    provided implementations. To do that, we followed the following train of thought
    (sadly unsuccesfully):

            1. We created our version of smartdoor in an IOLTS way, called
            'correctIOLTS'.
            2. We wanted to convert every implementation that was provided
            into an IOLTS. We noticed that we needed a function which would
            convert every implementation into an IOLTS. This is what made
            us drop the implementation, as we were unable to think of how to
            do it (time problems).
            3. If we had been successful on the idea of converting implementations
            to IOLTS, then we could have implemented an 'out' function
            that return all the output labels for a set of traces.
            4. Having that, we would have created an 'ioco' function, which
            would have checked if the out labels coming from the implementation 
            is a subset of the out labels from our model after an specific
            trace. If so, we would return true, if not, then false. In terms of
            error handeling, we were not able to think that far ahead.
-}

-- > Door Implementations as IOLTS --------------------------------------------
{-
    0 = closed
    1 = opened
    2 = locked
-}

-- correctIOLTS :: IOLTS
-- correctIOLTS = createIOLTS [
--         (0,"?open",1),
--         (0,"!opened",1),
--         (1,"?close",0),
--         (1,"?closed",0),
--         (0,"?lock",2),
--         (0,"!locked",2)
--         (2,"?unlock",0)
--         (2,"!unlocked",0)
--     ]

-- implToIOLTS :: (State -> Label -> (State, Label)) -> IOLTS
-- implToIOLTS = undefined

-- testLTSAgainstSUT :: IOLTS -> (State -> Label -> (State, Label)) -> Bool
-- testLTSAgainstSUT model@(q,il,ol,t,q0) impl = undefined 

-- ioco :: IOLTS -> IOLTS -> Trace -> Bool
-- ioco model impl trace = undefined

-- out :: IOLTS -> [State] -> [Label]
-- out l@(q,il,ol,t,q0) q1 = map snd (filter (f q1) t) 
--     where f q1 (s1, output_label, s2) = s1 'elem' q1

main :: IO ()
main = putStrLn "Does not work :("
