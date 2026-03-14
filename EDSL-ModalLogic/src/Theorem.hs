module Theorem where

import Types
import Monad
import Parser
import Eval

checkTheorem :: String -> Maybe [LabeledForm]
checkTheorem s = let form = parseForm "Syntax" s
                 in case form of
                      Left _ -> Nothing
                      Right f ->
                        let result = runState checkPends (SC [(0,Not f)] [] [] [0] 0)
                        in case result of
                            [] -> Nothing
                            ((_,(SC _ t _ _ _)):_) -> (Just t)