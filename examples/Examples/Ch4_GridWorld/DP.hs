module Examples.Ch4_GridWorld.DP where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import RL.Types
import RL.Imports
import RL.DP

import Examples.Ch4_GridWorld.Rules as GW


instance (Fractional num, Ord num) => DP_Problem (GW num) (Int,Int) Action num where

  rl_states p@(GW (sx,sy) _) = Set.fromList [(x,y) | x <- [0..sx-1], y <- [0..sy-1]]

  rl_actions pr s =
    case Set.member s (rl_terminal_states pr) of
      True -> Set.empty
      False -> Set.fromList [minBound..maxBound]

  rl_transitions gw@(GW (sx,sy) _) s@(x,y) a =
    Set.fromList [(GW.transition gw s a,1%1)]

  rl_reward (GW (sx,sy) _) s a s' = -1

  rl_terminal_states (GW _ exits) = exits


instance (Fractional num, Ord num) => DP_Policy (GW num) GWRandomPolicy (Int,Int) Action num where

  rlp_action GWRandomPolicy g s = (\x -> (x,1%(toInteger $ length a)))`Set.map`a
    where
      a = rl_actions g s

showStateVal gw StateVal{..} = showGW gw (\p -> Map.lookup p v_map)

-- | Evaluate random policy with DP method
gw_eval_dp :: (Fractional num, Ord num, Real num) => GW num -> IO (StateVal num (Int,Int))
gw_eval_dp gw =
  let
    opts = defaultOpts{eo_max_iter=300, eo_gamma = 1, eo_etha = 0.001}
    p = GWRandomPolicy
  in do
  v <- policy_eval gw p opts (zero_sate_values gw)
  showStateVal gw v
  p' <- policy_improve gw opts v
  showGenericPolicy gw p'
  return v

-- | Calculate Best policy, print its value function
gw_iter_dp :: (Fractional num, Ord num, Real num) => GW num -> IO (StateVal num (Int,Int))
gw_iter_dp gw =
  let
    opts = defaultOpts{eo_max_iter=300, eo_gamma = 1, eo_etha = 0.001, eo_debug = const (return ())}
    v0 = zero_sate_values gw
    p0 = GWRandomPolicy
  in do
  (v',p') <- policy_iteraton gw p0 v0 opts
  showStateVal gw v'
  return v'


