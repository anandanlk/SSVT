{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Exercise6 where

{-

---- EXERCISE 6 ---------------------------------------------------------------

    > Time Spent: 15h

    > Description:

        We are asked to write a function that converts a non cnf function into
    CNF form.

        cnf :: Form -> Form

        To do this we followed two different implementation directions: a first
    one that used a single function, and a second one were we divide the cnf
    into several smaller functions.

         - Implementation 1: our cnf'function implements the arrowfree and 
                             nnf functions in one go. We thought this implementation
                             would work at first. However, we encountered a 
                             problem with forms that required factorization
                             (like, for example, 'form3').

                             This implementation can be run using "main'".

         - Implementation 2: after encountering the previous problem we seeked
                             for a second, more modular solution, that could help
                             us find where our problem was. 
                             
                             We noticed that we needed to use distributivity
                             and then associativity. For that, we wrote a function 
                             'rundis' that deals with the distribution step. This
                             function calls our distributivity function 'dis' several
                             times. Why? Let's consider the following example:
                             
                                ((p & \q) | (q & \r)) | (\p | r)
                                |___________________|   |______|
                                    Bracket 1           Bracket 2
                                
                                If we would run 'dis' only one time, it would
                                only be runned in the first parenthesis, and 
                                therefore, the second bracket would never get
                                distributed in the first one.
                             
                             In case of the associativity, we wrote a function called 'ass'
                             that eliminates the useless parenthesis.

                             Finally, we added two more functions called 'dsj' and 'cnj'
                             to get rid of tautologies and contradictions.

                             This implementation can be run using "main", and it is
                             the working implementation.

                             It is also worth noting that we created some extra functions
                             that we did not use at the end. We included them at the end of 
                             this file, in the section called 'Unused functions'.

    > Testing: 

        We have created several examples which we have either solved in class or 
    on our own to test the correctness of our code. 

-}

-- > Given Code ---------------------------------------------------------------
type Name = Int
data Form = Prop Name | T | F
    | Neg Form
    | Cnj [Form]
    | Dsj [Form]
    | Impl Form Form
    | Equiv Form Form
    deriving (Eq,Ord)

instance Show Form where
    show T = "T"
    show F = "F"
    show (Prop x) = show x
    show (Neg f) = '-' : show f
    show (Cnj fs) = "*(" ++ showLst fs ++ ")" --and 
    show (Dsj fs) = "+(" ++ showLst fs ++ ")" --or
    show (Impl f1 f2) = "(" ++ show f1 ++ "==>" ++ show f2 ++ ")"
    show (Equiv f1 f2) = "(" ++ show f1 ++ "<=>" ++ show f2 ++ ")"

showLst,showRest :: [Form] -> String
showLst [] = ""
showLst (f:fs) = show f ++ showRest fs
showRest [] = ""
showRest (f:fs) = ' ': show f ++ showRest fs

-- > Variables ----------------------------------------------------------------
p, q, r :: Form
p = Prop 1
q = Prop 2
r = Prop 3

form1, form2, form3, form4, form5:: Form
form1 = Neg (Neg (Neg (Prop 1)))
form2 = Neg (Dsj [Prop 1,Neg (Prop 2)])
form3 = Neg (Cnj [Neg (Prop 1), Neg (Prop 2)])
form4 = Equiv (Impl p q) (Impl (Neg q) (Neg p))
form5 = Impl (Cnj [Impl p q, Impl q r]) (Impl p r)


-------------------------------------------------------------------------------
-- > IMPLEMENTATION 1 ---------------------------------------------------------
-------------------------------------------------------------------------------

-- > Function       -----------------------------------------------------------
cnf' :: Form -> Form
cnf' (Prop f1) = Prop f1
cnf' (Neg (Prop f1)) = Neg (Prop f1)
cnf' (Neg (Neg f1)) = f1
cnf' (Neg (Cnj fs)) = cnf' (Dsj (map (Neg . cnf') fs))
cnf' (Neg (Dsj fs)) = cnf' (Cnj (map (Neg . cnf') fs))
cnf' (Dsj [f1, Cnj [f2, f3]]) = Cnj [cnf' (Dsj [cnf' f1, cnf' f2]), cnf' (Dsj [cnf' f1, cnf' f3])]
cnf' (Dsj [Cnj [f2, f3], f1]) = Cnj [cnf'(Dsj [cnf' f2, cnf' f1]), cnf'(Dsj [cnf' f3, cnf' f1])]
cnf' (Dsj [f1, Dsj [f2, f3]]) = Dsj [cnf' f1, cnf' f2, cnf' f3]
cnf' (Dsj [Dsj [f2, f3], f1]) = Dsj [cnf' f1, cnf' f2, cnf' f3]
cnf' (Cnj fs) = Cnj (map cnf' fs)
cnf' (Dsj fs) = Dsj (map cnf' fs)
cnf' (Impl f1 f2) = cnf' (Dsj [Neg (cnf' f1), cnf' f2])
cnf' (Equiv f1 f2) = cnf' $ Cnj [Dsj [cnf' f1, Neg (cnf' f2)], Dsj [Neg (cnf' f1), cnf' f2]]

-- > Main           -----------------------------------------------------------
main' :: IO ()
main' = putStrLn $ show (cnf'(Neg (Neg (Neg (Prop 1))))) ++ "\n"
    ++ show (cnf' (Neg (Dsj [Prop 1,Neg (Prop 2)]))) ++ "\n"
    ++ show (cnf' (Neg (Cnj [Neg (Prop 1), Neg (Prop 2)]))) ++ "\n"
    ++ show (cnf' form1) ++ "\n"
    ++ show (cnf' form2) ++ "\n"
    ++ show (cnf' form3)

-------------------------------------------------------------------------------
-- > IMPLEMENTATION 2 ---------------------------------------------------------
-------------------------------------------------------------------------------

-- > Function -----------------------------------------------------------------

-- Arrow Free
arrowfree :: Form -> Form
arrowfree (Prop x) = Prop x
arrowfree (Neg f) = Neg (arrowfree f)
arrowfree (Cnj fs) = Cnj (map arrowfree fs)
arrowfree (Dsj fs) = Dsj (map arrowfree fs)
arrowfree (Impl f1 f2) = Dsj [Neg (arrowfree f1), arrowfree f2]
arrowfree (Equiv f1 f2) = Cnj [arrowfree (Impl f1 f2), arrowfree (Impl f2 f1)]


-- NNF
nnf :: Form -> Form
nnf (Prop x) = Prop x
nnf (Neg (Prop x)) = Neg (Prop x)
nnf (Neg (Neg f)) = nnf f
nnf (Cnj fs) = Cnj (map nnf fs)
nnf (Dsj fs) = Dsj (map nnf fs)
nnf (Neg (Cnj fs)) = Dsj (map (nnf.Neg) fs)
nnf (Neg (Dsj fs)) = Cnj (map (nnf.Neg) fs)

-- Running distributivity several times until it doesn't change
rundis :: Form -> Form
rundis x | x == dis x = x
         | otherwise = rundis (dis x)

-- Distributivity
dis :: Form -> Form
dis (Dsj [f1, Cnj fs]) = dis $ Cnj (map (\x -> Dsj [f1, x]) fs)
dis (Dsj [Cnj fs, f1]) = dis $ Cnj (map (\x -> Dsj [x, f1]) fs)
dis (Prop f1) = Prop f1
dis (Neg f1) = Neg f1
dis (Cnj fs) = Cnj (map dis fs)
dis (Dsj fs) = Dsj (map dis fs)

-- Associativity
ass :: Form -> Form
ass (Cnj [Cnj f1, Cnj f2]) = Cnj (map ass f1 ++ map ass f2)
ass (Dsj [Dsj f1, Dsj f2]) = Dsj (map ass f1 ++ map ass f2)
ass (Cnj [Cnj f1, f2]) = Cnj (map ass f1 ++ [f2])
ass (Cnj [f1, Cnj f2]) = Cnj (f1 : map ass f2)
ass (Dsj [Dsj f1, f2]) = Dsj (map ass f1 ++ [f2])
ass (Dsj [f1, Dsj f2]) = Dsj (f1 : map ass f2)
ass (Prop f1) = Prop f1
ass (Neg f1) = Neg f1
ass (Cnj fs) = Cnj (map ass fs)
ass (Dsj fs) = Dsj (map ass fs)

dsj :: Form -> Form
dsj (Cnj fs) = Cnj (map dsj fs)
dsj (Dsj fs) | any  (\x -> Neg x `elem` fs) fs = T
             | otherwise = Dsj fs
dsj (Prop f1) = Prop f1
dsj (Neg f1) = Neg f1

cnj :: Form -> Form 
cnj (Cnj fs) | any  (\x -> Neg x `elem` fs) fs = F
             | all (== T) fs = T
             | otherwise = Cnj fs
cnj (Prop f1) = Prop f1
cnj (Neg f1) = Neg f1
cnj (Dsj fs) = Dsj fs

-- CNF
cnf :: Form -> Form
cnf = cnj . dsj . ass . rundis . nnf . arrowfree

-- > Main            ----------------------------------------------------------
main :: IO ()
main = do
    putStrLn ">>> ¬(¬(¬p)):"
    print $ cnf form1

    putStrLn "\n>>> ¬(p ∨ (¬q)):"
    print $ cnf form2

    putStrLn "\n>>> ¬((¬p) ∧ (¬q)):"
    print $ cnf form3

    putStrLn "\n>>> (p → q) ↔ ((¬q) → (¬p)):"
    print $ cnf form4

    putStrLn "\n>>> ((p → q) ∧ (q → r)) → (p → r):"
    print $ cnf form5


-- > Unused functions ---------------------------------------------------------
-- commutativity :: Form -> Form
-- commutativity (Prop f1) = Prop f1
-- commutativity (Neg f1) = Neg (commutativity f1)
-- commutativity (Cnj fs) = Cnj (map commutativity (sort fs))
-- commutativity (Dsj fs) = Dsj (map commutativity (sort fs))

-- idempotent :: Form -> Form
-- idempotent (Prop f1) = Prop f1
-- idempotent (Neg f1) = Neg (idempotent f1)
-- idempotent (Cnj xs) | length (nub xs) <= 1 = head (nub xs)
--                     | otherwise = Cnj (map idempotent xs)
-- idempotent (Dsj xs) | length (nub xs) <= 1 = head (nub xs)
--                     | otherwise = Dsj (map idempotent xs)
