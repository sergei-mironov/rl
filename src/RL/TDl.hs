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
  , o_q0 :: TD_Number
  , o_lambda :: TD_Number
  } deriving (Show)

type Q s a = M s a
type Z s a = M s a

data TDl_State s a = TDl_State {
    _tdl_q :: Q s a
  , _tdl_z :: Z s a
  }

$(makeLenses ''TDl_State)

initialState :: TD_Number -> TDl_State s a
initialState x0 = TDl_State (initM x0) (initM 0)

class (Eq s, Hashable s, Show s, Eq a, Hashable a, Enum a, Bounded a, Show a) =>
    TDl_Problem pr m s a | pr -> m, pr -> s , pr -> a where
  td_is_terminal :: pr -> s -> Bool
  td_greedy :: pr -> Bool -> a -> a
  td_transition :: pr -> s -> a -> TDl_State s a -> m (s,TD_Number)
  td_modify :: pr -> s -> a -> TDl_State s a  -> m ()

queryQ s = HashMap.toList <$> get_s s <$> use tdl_q
modifyQ pr s a f = tdl_q %= modify_s_a s a f
listZ pr s a f = (list <$> use tdl_z) >>= mapM_ f >> get >>= lift . td_modify pr s a
modifyZ pr s a f = tdl_z %= modify_s_a s a f
action pr s eps = queryQ s >>= eps_greedy_action eps (td_greedy pr)
transition pr s a = get >>= lift . td_transition pr s a
loopM s0 f m = iterateUntilM (not . f) m s0

sarsa_lambda :: (MonadRnd g m, TDl_Problem pr m s a)
  => s -> TDl_Opts -> pr -> m s
sarsa_lambda s0 TDl_Opts{..} pr = do
  flip evalStateT (initialState o_q0) $ do
    (a0,q0) <- action pr s0 o_eps
    fst <$> do
      loopM (s0,(a0,q0)) (not . td_is_terminal pr . fst) $ \(s,(a,q)) -> do
        (s',r) <- transition pr s a
        (a',q') <- action pr s' o_eps
        delta <- pure $ r + o_gamma * q' - q
        modifyZ pr s a (+1)
        listZ pr s a $ \(s,a,z) -> do
          modifyQ pr s a (\q -> q + o_alpha * delta * z)
          modifyZ pr s a (const $ o_gamma * o_lambda * z)
        return (s',(a',q'))

