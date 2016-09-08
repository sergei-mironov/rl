{-# LANGUAGE DeriveFunctor #-}
module RL.TD where

import Data.HashMap.Strict as HashMap
import Control.Monad.Loops

import RL.Imports
import RL.TD.Class (eps_greedy_action)
import RL.TD.Types

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

type Q s a = M s a

class (Eq s, Hashable s, Show s, Eq a, Hashable a, Enum a, Bounded a, Show a) =>
    TD_Problem pr m s a | pr -> m, pr -> s , pr -> a where
  td_is_terminal :: pr -> s -> Bool
  td_greedy :: pr -> Bool -> a -> a
  td_transition :: pr -> s -> a -> Q s a -> m (s,TD_Number)

queryQ s = HashMap.toList <$> get_s s <$> get
modifyQ s a f = modify (modify_s_a s a f)
action pr s eps = (get_s s <$> get) >>= eps_greedy_action eps (td_greedy pr)
transition pr s a = get >>= lift . td_transition pr s a
loopM s0 f m = iterateUntilM f m s0

-- | Q-Learning algorithm
qlearn :: (MonadRnd g m, TD_Problem pr m s a) => Q_Opts -> Q s a -> s -> pr -> m (s, Q s a)
qlearn Q_Opts{..} q0 s0 pr = do
  flip runStateT q0 $ do
    loopM s0 (not . td_is_terminal pr) $ \s -> do
      (a,_) <- action pr s o_eps
      (s',r) <- transition pr s a
      max_qs' <- snd . maximumBy (compare`on`snd) <$> queryQ s'
      modifyQ s a $ \q -> q + o_alpha * (r + o_gamma * max_qs' - q)
      return s'

-- | Execute Q-actions without learning
qexec :: (MonadRnd g m, TD_Problem pr m s a) => Q_Opts -> Q s a -> s -> pr -> m s
qexec Q_Opts{..} q0 s0 pr = do
  flip evalStateT q0 $ do
  loopM s0 (not . td_is_terminal pr) $ \s -> do
    a <- fst . maximumBy (compare`on`snd) <$> queryQ s
    (s',_) <- transition pr s a
    return s'

