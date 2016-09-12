module RL.MC (
    module RL.MC
  , module RL.MC.Types
  ) where

import RL.Imports
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Prelude hiding(break)

import RL.Types as RL
import RL.DP (DP_Problem(..))
import qualified RL.DP as DP

import RL.MC.Types

-- | Builds an episode which is a list of transitions, terminal transition is near head
episode :: (MonadRnd g m , MC_Policy num pr s a p, MonadIO m, Show s, Show a)
  => pr num -> s -> p -> m (Episode s a)
episode pr s p = do
  Episode . snd <$> do
  flip execStateT (s,[]) $ do
  loop $ do
    s <- gets fst
    a <- roll $ mc_action pr s p
    (s', term) <- roll $ mc_transition pr s a
    modify $ const s' *** ((s,a,s'):)
    when term $ do
      break ()

-- Backtrack rewards, first visit counts
backtrack_fv :: (MC_Problem num pr s a) => pr num -> Episode s a -> Map s num
backtrack_fv pr ep =
  view _1 $ flip execState (Map.empty, 0) $ do
    forM_ (episode_backward ep) $ \ (s,a,s') -> do
      r <- pure $ mc_reward pr s a s'
      _2 %= (+r)
      g <- use _2
      _1 %= (Map.insert s g)



data EvalState num s = EvalState {
    _es_v :: Map s (Avg num)
  , _es_iter :: Integer
  } deriving(Show)

makeLenses ''EvalState

initialEvalState :: (Ord s, Fractional num) => EvalState num s
initialEvalState = EvalState mempty 0

-- | DIfference between state value estimates
-- FIXME: handle missing states case
diffVal :: (Ord s, Fractional num) => StateVal num s -> (Map s (Avg num)) -> num
diffVal (v_map -> tgt) src = sum $ Map.intersectionWith (\a b -> abs $ a - (current b)) tgt src


data E_Ext num s = E_Ext {
    eo_learnMonitor :: Maybe (Monitor num s)
}

type EvalOpts num s = Opts num (E_Ext num s)

defaultEvalOpts :: (Fractional num) => EvalOpts num s
defaultEvalOpts = defaultOpts E_Ext {
    eo_learnMonitor = Nothing
  }


-- Monte carlo policy evaluation, Figure 5.1. pg 109
policy_eval :: (MC_Policy_Show num pr s a p, RandomGen g, MonadIO m, Real num)
  => EvalOpts num s -> pr num -> p -> g -> m (StateVal num s, g)
policy_eval Opts{..} pr p = do
  runRndT $ do
  StateVal . Map.map current . view es_v <$> do
  flip execStateT initialEvalState $ do
  loop $ do
    i <- use es_iter
    es_iter %= (+1)
    when (i > o_max_iter-1) $ do
      break ()

    -- let rnd = lift . lift . liftRandT
    ss <- roll $ mc_state_nonterm pr
    es <- episode pr ss p
    gs <- pure $ backtrack_fv pr es

    forM_ (Map.toList gs) $ \(s,g) -> do
      v <- fromMaybe initialAvg <$> uses es_v (Map.lookup s)
      es_v %= Map.insert s (meld v g)

    case eo_learnMonitor o_ext of
      Nothing -> return ()
      Just Monitor{..} -> do
        v <- use es_v
        pushData mon_data (fromInteger i) (diffVal mon_target v)

