{-# LANGUAGE DeriveFunctor #-}
module RL.TD where

import qualified Prelude
import qualified Data.HashMap.Strict as HashMap

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
  , o_gamma = 1.0
  , o_eps = 0.3
  }

type Q s a = M s a
type V s = HashMap s TD_Number

emptyQ :: TD_Number -> Q s a
emptyQ = initM

q2v :: (Bounded a, Enum a, Eq a, Hashable a, Eq s, Hashable s) => Q s a -> V s
q2v = foldMap_s (\(s,l) -> HashMap.singleton s (maximum l))

-- FIXME: handle missing states case
diffV :: (Eq s, Hashable s) => V s -> V s -> TD_Number
diffV tgt src = sum (HashMap.intersectionWith (\a b -> abs (a - b)) tgt src)

class (Monad m, Eq s, Hashable s, Show s, Eq a, Hashable a, Enum a, Bounded a, Show a) =>
    TD_Problem pr m s a | pr -> m, pr -> s , pr -> a where
  td_is_terminal :: pr -> s -> Bool
  td_greedy :: pr -> Bool -> a -> a
  td_reward :: pr -> s -> a -> s -> TD_Number
  td_transition :: pr -> s -> a -> Q s a -> m s
  td_modify :: pr -> s -> a -> Q s a  -> m ()

queryQ s = HashMap.toList <$> get_s s <$> get
modifyQ pr s a f = modify (modify_s_a s a f) >> get >>= lift . td_modify pr s a
action pr s eps = (get_s s <$> get) >>= eps_greedy_action eps (td_greedy pr)
transition pr s a = get >>= lift . td_transition pr s a
loopM s0 f m = iterateUntilM (not . f) m s0

-- | Q-Learning algorithm
qlearn :: (MonadRnd g m, TD_Problem pr m s a) => Q_Opts -> Q s a -> s -> pr -> m (s, Q s a)
qlearn Q_Opts{..} q0 s0 pr = do
  flip runStateT q0 $ do
  loopM s0 (not . td_is_terminal pr) $ \s -> do
    (a,_) <- action pr s o_eps
    s' <- transition pr s a
    r <- pure $ td_reward pr s a s'
    max_qs' <- snd . maximumBy (compare`on`snd) <$> queryQ s'
    modifyQ pr s a $ \q -> q + o_alpha * (r + o_gamma * max_qs' - q)
    return s'

-- | Q-Executive algorithm. Actions are taken greedily, no learning is performed
qexec :: (MonadRnd g m, TD_Problem pr m s a) => Q_Opts -> Q s a -> s -> pr -> m s
qexec Q_Opts{..} q0 s0 pr = do
  flip evalStateT q0 $ do
  loopM s0 (not . td_is_terminal pr) $ \s -> do
    a <- fst . maximumBy (compare`on`snd) <$> queryQ s
    s' <- transition pr s a
    return s'

