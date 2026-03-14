module Eval where

import Types
import Monad
import Control.Monad                  (liftM, ap)
import Data.List                      (intersect)


hayContradiccion :: [LabeledForm] -> Bool
hayContradiccion [] = False
hayContradiccion lits = let atom    = [(w, f) | (w, Var f) <- lits]
                            notAtom = [(w, f) | (w, Not (Var f)) <- lits]
                        in not $ null (intersect atom notAtom)

checkPends :: Prover ()
checkPends = do (SC pend lits univ wlds next) <- getState
                if (hayContradiccion lits) then lift []
                else case pend of
                      []     -> return ()
                      (f:fs) -> do putState (SC fs lits univ wlds next)
                                   evalForm f

evalForm :: LabeledForm -> Prover ()

-- Constantes: Si es True seguimos, si es Bottom conrtamos la rama
evalForm (_, FTrue)               = checkPends
evalForm (_, FBottom)             = lift []
evalForm (_, Not FBottom)         = checkPends
evalForm (_, Not FTrue)           = lift []

-- Los literales van a la lista de literales
evalForm (w, Var p)               = do (SC pend lits univ wlds next) <- getState
                                       putState (SC pend ((w, Var p):lits) univ wlds next)
                                       checkPends
evalForm (w, Not (Var p))         = do (SC pend lits univ wlds next) <- getState
                                       putState (SC pend ((w, Not (Var p)):lits) univ wlds next)
                                       checkPends

-- Equivalencia logica: ~ (~ p) = p
evalForm (w, Not (Not f))         = do (SC pend lits univ wlds next) <- getState
                                       putState (SC ((w,f):pend) lits univ wlds next)
                                       checkPends

-- Casos normales: No ramifican la demostracion

evalForm (w, And f1 f2)           = do (SC pend lits univ wlds next) <- getState
                                       putState (SC ((w,f1):(w,f2):pend) lits univ wlds next)
                                       checkPends

-- Equivalencia logica: ~ (p || q) = ~p && ~q
evalForm (w, Not (Or f1 f2))      = do (SC pend lits univ wlds next) <- getState
                                       putState (SC ((w, Not f1):(w, Not f2):pend) lits univ wlds next)
                                       checkPends

-- Equivalencia logica: ~ (p -> q) = p && ~q
evalForm (w, Not (Implies f1 f2)) = do (SC pend lits univ wlds next) <- getState
                                       putState (SC ((w, f1):(w, Not f2):pend) lits univ wlds next)
                                       checkPends

-- Casos que ramifican la demostracion en dos:

evalForm (w, Or f1 f2)            = do (SC pend lits univ wlds next) <- getState
                                       let branch1 = (SC ((w,f1):pend) lits univ wlds next)
                                           branch2 = (SC ((w,f2):pend) lits univ wlds next)
                                       b <- lift [branch1, branch2]
                                       putState b
                                       checkPends

-- Equivalencia logica: p -> q = ~p || q
evalForm (w, Implies f1 f2)       = do (SC pend lits univ wlds next) <- getState
                                       let branch1 = (SC ((w, Not f1):pend) lits univ wlds next)
                                           branch2 = (SC ((w, f2):pend) lits univ wlds next)
                                       b <- lift [branch1, branch2]
                                       putState b
                                       checkPends

-- Equivalencia logica: ~ (p && q) = ~p || ~q
evalForm (w, Not (And f1 f2))     = do (SC pend lits univ wlds next) <- getState
                                       let branch1 = (SC ((w, Not f1):pend) lits univ wlds next)
                                           branch2 = (SC ((w, Not f2):pend) lits univ wlds next)
                                       b <- lift [branch1, branch2]
                                       putState b
                                       checkPends

-- Operadores modales

evalForm (_, Dia f)               = do (SC pend lits univ wlds next) <- getState
                                       let newWorld = next + 1
                                           heritage = map (\form -> (newWorld,form)) univ
                                           newPends = (newWorld,f) : heritage ++ pend
                                       putState (SC newPends lits univ (newWorld:wlds) newWorld)
                                       checkPends

evalForm (_, Box f)               = do (SC pend lits univ wlds next) <- getState
                                       let copyToAll = map (\wl -> (wl, f)) wlds
                                           newPends = copyToAll ++ pend
                                       putState (SC newPends lits (f:univ) wlds next)
                                       checkPends
                                
-- Equivalencia logica: ~<>p = []~p 
evalForm (_, Not (Dia f))         = do (SC pend lits univ wlds next) <- getState
                                       let copyToAll = map (\wl -> (wl, Not f)) wlds
                                           newPends = copyToAll ++ pend
                                       putState (SC newPends lits (Not f:univ) wlds next)
                                       checkPends

-- Equivalencia logica: ~[]p = <>~p
evalForm (_, Not (Box f))         = do (SC pend lits univ wlds next) <- getState
                                       let newWorld = next + 1
                                           heritage = map (\form -> (newWorld,form)) univ
                                           newPends = (newWorld, Not f) : heritage ++ pend
                                       putState (SC newPends lits univ (newWorld:wlds) newWorld)
                                       checkPends