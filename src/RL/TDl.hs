{-# LANGUAGE DeriveFunctor #-}
module RL.TDl where

import qualified Data.List as List
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

import Control.Monad.Trans.Free.Church
import RL.Imports
import RL.TD.Types
import RL.TD.Class (eps_greedy_action)

data TDl_Opts = TDl_Opts {
    o_alpha :: TD_Number
  , o_gamma :: TD_Number
  , o_eps :: TD_Number
  , o_lambda :: TD_Number
  } deriving (Show)

type Q s a = M s a
type Z s a = M s a
type V s a = HashMap s (a, TD_Number)

emptyQ :: TD_Number -> Q s a
emptyQ = initM

q2v :: (Bounded a, Enum a, Eq a, Hashable a, Eq s, Hashable s) => Q s a -> V s a
q2v = foldMap_s (\(s,l) -> HashMap.singleton s (layer_s_max l))

-- FIXME: handle missing states case
diffV :: (Eq s, Hashable s) => V s a -> V s a -> TD_Number
diffV tgt src = sum (HashMap.intersectionWith (\a b -> abs ((snd a) - (snd b))) tgt src)


data TDl_State s a = TDl_State {
    _tdl_q :: Q s a
  , _tdl_z :: Z s a
  }

$(makeLenses ''TDl_State)

initialState :: Q s a -> TDl_State s a
initialState q0 = TDl_State q0 (initM 0)

class (Eq s, Hashable s, Show s, Eq a, Hashable a, Enum a, Bounded a, Show a) =>
    TDl_Problem pr m s a | pr -> m, pr -> s , pr -> a where
  td_is_terminal :: pr -> s -> Bool
  td_greedy :: pr -> Bool -> a -> a
  td_transition :: pr -> s -> a -> TDl_State s a -> m s
  td_reward :: pr -> s -> a -> s -> TD_Number
  td_modify :: pr -> s -> a -> TDl_State s a  -> m ()

queryQ s = HashMap.toList <$> get_s s <$> use tdl_q
modifyQ pr s a f = tdl_q %= modify_s_a s a f
listZ pr s a f = (list <$> use tdl_z) >>= mapM_ f >> get >>= lift . td_modify pr s a
modifyZ pr s a f = tdl_z %= modify_s_a s a f
action pr s eps = queryQ s >>= eps_greedy_action eps (td_greedy pr)
transition pr s a = get >>= lift . td_transition pr s a

-- | TD(lambda) learning, aka Sarsa, pg 171
tdl_learn :: (MonadRnd g m, TDl_Problem pr m s a)
  => TDl_Opts -> Q s a -> s -> pr -> m (s, Q s a)
tdl_learn TDl_Opts{..} q0 s0 pr = do
  (view _1 *** view tdl_q) <$> do
  flip runStateT (initialState q0) $ do
    (a0,q0) <- action pr s0 o_eps
    loopM (s0,a0,q0) (not . td_is_terminal pr . view _1) $ \(s,a,q) -> do
      s' <- transition pr s a
      r <- pure $ td_reward pr s a s'
      (a',q') <- action pr s' o_eps
      delta <- pure $ r + o_gamma * q' - q
      modifyZ pr s a (+1)
      listZ pr s a $ \(s,a,z) -> do
        modifyQ pr s a (\q -> q + o_alpha * delta * z)
        modifyZ pr s a (const $ o_gamma * o_lambda * z)
      return (s',a',q')


-- | Watkins's Q(lambda) learning algorithm, pg 174
qlw_learn :: (MonadRnd g m, TDl_Problem pr m s a)
  => TDl_Opts -> Q s a -> s -> pr -> m (s, Q s a)
qlw_learn TDl_Opts{..}  q0 s0 pr =
  (view _1 *** view tdl_q) <$> do
  flip runStateT (initialState q0) $ do
    (a0,q0) <- action pr s0 o_eps
    loopM (s0,a0,q0) (not . td_is_terminal pr . view _1) $ \(s,a,q) -> do
      s' <- transition pr s a
      r <- pure $ td_reward pr s a s'
      (a',q') <- action pr s' o_eps
      (a'',q'') <- maximumBy (compare`on`snd) <$> queryQ s'
      delta <- pure $ r + o_gamma*q'' - q
      modifyZ pr s a (+1)
      listZ pr s a $ \(s,a,z) -> do
        modifyQ pr s a (\q -> q + o_alpha * delta * z)
        modifyZ pr s a (\z -> if a' == a''
                                then o_gamma*o_lambda*z
                                else 0)
      return (s',a',q')


