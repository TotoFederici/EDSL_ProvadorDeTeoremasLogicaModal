module PrintChart where

import Types
import Monad
import Parser
import Eval
import Theorem
import PP

import Data.List                      (nub)

counterExample :: [LabeledForm] -> World -> IO ()
counterExample l w | w < 0 = putStrLn "--------------------------------"
                   | otherwise = let forms = [f | (w', f) <- l, w == w']
                                 in if null forms
                                    then (counterExample l (w-1))
                                    else do putStrLn "--------------------------------"
                                            putStrLn ("Formulas validas en el mundo " ++ show w)
                                            chartPerWorld (nub forms)
                                            (counterExample l (w-1))

chartPerWorld :: [Form] -> IO ()
chartPerWorld []     = return ()
chartPerWorld (l:ls) = do putStrLn (renderForm l)
                          chartPerWorld ls