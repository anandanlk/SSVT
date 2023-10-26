module Exercise6 where
import Data.List
import LTS
import Test.QuickCheck

visualizeLTS :: LTS -> String
visualizeLTS (q, l, lt, q0) = unlines $ map showRow [1..maxState]
  where
    maxState = maximum $ q:[s' | (s, _, s') <- lt] ++ [q0]
    showRow s = concat [showState s l | l <- l]
    showState s l = case lookup (s, l, s') lt of
      Just _ -> "+---+"
      Nothing -> "     "

main :: IO()
main = do
    putStrLn ">>> Visualizing LTS:"
    putStrLn $ visualizeLTS tretmanV