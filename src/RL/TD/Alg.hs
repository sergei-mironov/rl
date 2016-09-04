{-# LANGUAGE DeriveFunctor #-}
module RL.TD.Alg where

import Control.Monad.Rnd as Rnd
import Control.Monad.Loops

import RL.Imports
import RL.TD.Class

data TD_AlgF s a next =
    InitialState (s -> next)
  | Query_Q s ([(a,TD_Number)] -> next)
  | Modify_Q s a (TD_Number -> TD_Number) next
  deriving(Functor)

makeFree ''TD_AlgF

data Q_Opts = Q_Opts {
    o_alpha :: TD_Number
  , o_gamma :: TD_Number
  , o_eps :: TD_Number
} deriving (Show)

defaultOpts = Q_Opts {
    o_alpha = 0.1
  , o_gamma = 0.5
  , o_eps = 0.3
  }

-- | Execute Q-actions without learning
qexecF :: (MonadRnd g m, TD_Problem pr s a, MonadFree (TD_AlgF s a) m) => Q_Opts -> pr -> m s
qexecF Q_Opts{..} pr = do
  initialState >>= do
  iterateUntilM (not . td_is_terminal pr) $ \s -> do
    a <- fst . maximumBy (compare`on`snd) <$> query_Q s
    s' <- pure $ td_transition pr s a
    return s'

-- | Execute Q-Learning algorithm
qlearnF :: (MonadRnd g m, TD_Problem pr s a, MonadFree (TD_AlgF s a) m) => Q_Opts -> pr -> m s
qlearnF Q_Opts{..} pr = do
  initialState >>= do
  iterateUntilM (td_is_terminal pr) $ \s -> do
    (a,_) <- query_Q s >>= eps_action o_eps pr
    s' <- pure $ td_transition pr s a
    r <- pure $ td_reward pr s a s
    max_qs' <- snd . maximumBy (compare`on`snd) <$> query_Q s'
    modify_Q s a $ \qs -> qs + o_alpha * (r + o_gamma * max_qs' - qs)
    return s'


