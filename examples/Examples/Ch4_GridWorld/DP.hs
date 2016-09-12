module Examples.Ch4_GridWorld.DP (
    module Examples.Ch4_GridWorld.DP
  ) where

import qualified Data.List as List
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set

import RL.Imports
import RL.DP as DP

import Examples.Ch4_GridWorld.Rules as GW

instance (Fractional num, Ord num) => DP_Problem (GW num) Point Action num where
  dp_states gw = GW.states gw
  dp_actions gw s = GW.actions gw s
  dp_transitions gw s a = Set.fromList [(GW.transition gw s a,1%1)]
  dp_reward gw s a s' = -1
  dp_terminal_states gw = gw_exits gw

-- | Evaluate random policy with DP method
gw_evalRandom_dp :: (DP_Problem (GW num) Point Action num) => GW num -> IO (V Point num)
gw_evalRandom_dp gw =
  let
    opts = defaultOpts{eo_max_iter=300, eo_gamma=1, eo_etha = 0.001}
    p0 = uniformPolicy gw
    v0 = initV gw 0
  in do
  policy_eval opts p0 v0 (DP gw $ \a b -> return ())

-- | Calculate Best policy, print its value function
gw_iter_dp :: (DP_Problem (GW num) Point Action num) => GW num -> IO (V Point num, P Point Action)
gw_iter_dp gw =
  let
    opts = defaultOpts{eo_max_iter=300, eo_gamma=1, eo_etha = 0.001}
    p0 = uniformPolicy gw
    v0 = initV gw 0
  in do
  policy_iteration opts p0 v0 (DP gw $ \a b -> return ())

-- | Calculate and show value function of random policy
gw_showRandom_dp gw = do
  v <- gw_evalRandom_dp gw
  showV gw (HashMap.toList v)

