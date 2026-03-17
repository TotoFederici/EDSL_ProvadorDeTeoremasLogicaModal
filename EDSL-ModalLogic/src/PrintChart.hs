module PrintChart where

import Types
import Monad
import Parser
import Eval
import Theorem
import PP

counterExample :: [LabeledForm] -> World -> IO ()
counterExample l w | w < 0 = putStrLn "--------------------------------"
                   | otherwise = let forms = [f | (w', f) <- l, w == w']
                                 in do putStrLn "--------------------------------"
                                       putStrLn ("Formulas validas en el mundo " ++ show w)
                                       chartPerWorld forms
                                       (counterExample l (w-1))

chartPerWorld :: [Form] -> IO ()
chartPerWorld []     = return ()
chartPerWorld (l:ls) = do putStrLn (renderForm l)
                          chartPerWorld ls
                          
-- [(0,Var "P"), (1, Var "Q"), (0, Not (Var "Q"))]