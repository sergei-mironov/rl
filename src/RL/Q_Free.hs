{-# LANGUAGE DeriveFunctor, DeriveAnyClass #-}
module RL.Q_Free where

import Control.Monad.Rnd as Rnd
import Control.Monad.Loops
-- import Control.Monad.Free
import Control.Monad.Trans.Free
import Control.Monad.Free.TH (makeFree)

import RL.Imports

type Q_Number = Double

class (Enum a, Bounded a, Eq a) => Q_Action a where
  q_mark_best :: Bool -> a -> a

class (Eq s) => Q_State s where
  q_is_final :: s -> Bool

class (Q_Action a, Q_State s) => Q_Problem s a | s -> a where
  q_reward :: s -> a -> s -> Q_Number

data Q_AlgF s a next =
    InitialState (s -> next)
  | Transition s a (s -> next)
  | Get_Actions s ([(a,Q_Number)] -> next)
  | Modify_Q s a (Q_Number -> Q_Number) next
  | Fin
  deriving(Functor)

-- Defines `initialState`, `transition`, etc.
makeFree ''Q_AlgF

data Q_Opts = Q_Opts {
    o_alpha :: Q_Number
  , o_gamma :: Q_Number
  , o_eps :: Q_Number
} deriving (Show)

defaultOpts = Q_Opts {
    o_alpha = 0.1
  , o_gamma = 0.5
  , o_eps = 0.3
  }

-- | Take eps-greedy action
qaction :: (MonadRnd g m, Q_Problem s a, MonadFree (Q_AlgF s a) m) => Q_Number -> s -> m a
qaction eps s = do

  qs <- get_Actions s

  let abest = fst $ maximumBy (compare`on`snd) qs
  let arest = map fst $ filter (\x -> fst x /= abest) qs

  join $ Rnd.fromList [
    swap (toRational (1.0-eps), do
      return (q_mark_best True abest)),
    swap (toRational eps, do
      r <- Rnd.uniform arest
      return (q_mark_best False r))
    ]

qexec :: (MonadRnd g m, Q_Problem s a, MonadFree (Q_AlgF s a) m) => Q_Opts -> m s
qexec Q_Opts{..} = do
  initialState >>= do
  iterateUntilM (not . q_is_final) $ \s -> do
    a <- qaction o_eps s
    s' <- transition s a
    return s'

qlearn :: (MonadRnd g m, Q_Problem s a, MonadFree (Q_AlgF s a) m) => Q_Opts -> m s
qlearn Q_Opts{..} = do
  initialState >>= do
  iterateUntilM (not . q_is_final) $ \s -> do
    a <- qaction o_eps s
    s' <- transition s a
    let r = q_reward s a s
    max_qs' <- snd . maximumBy (compare`on`snd) <$> get_Actions s'
    modify_Q s a $ \qs -> qs + o_alpha * (r + o_gamma * max_qs' - qs)
    return s'

