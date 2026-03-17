module Theorem where

import Types
import Monad
import Parser
import Eval

checkTheorem :: Form -> Maybe ([LabeledForm], World)
checkTheorem f = let result = runState checkPends (SC [(0,Not f)] [] [] [0] 0)
                 in case result of
                      [] -> Nothing
                      ((_,(SC _ t _ _ wlds)):_) -> (Just (t, wlds))