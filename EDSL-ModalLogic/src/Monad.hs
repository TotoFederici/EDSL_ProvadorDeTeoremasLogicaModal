module Monad where

import Types
import Control.Monad                  (liftM, ap)

newtype State s m a = State {runState :: s -> m (a, s)}

instance Monad m => Monad (State s m) where
  return x = State (\s -> return (x,s))
  m >>= f  = State (\s -> let t = runState m s
                          in do (v,s') <- t
                                runState (f v) s')

instance Monad m => Functor (State s m) where
  fmap = liftM

instance Monad m => Applicative (State s m) where
  pure  = return
  (<*>) = ap

class MonadTrans t where
    lift :: Monad m => m a -> t m a

instance MonadTrans (State s) where
    lift m = State (\s -> do a <- m
                             return (a,s))

getState :: Monad m => State s m s
getState = State (\s -> return (s,s))

putState :: Monad m => s -> State s m ()
putState s = State (\_ -> return ((),s))

type Prover a = State StateChart [] a