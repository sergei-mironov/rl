module Examples.Ch4_GridWorld.DP where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import RL.Types
import RL.Imports
import RL.DP

import Examples.Ch4_GridWorld.Base


instance (Fractional num, Ord num) => DP_Problem num GW (Int,Int) Action where

  rl_states p@(GW (sx,sy) _) = Set.fromList [(x,y) | x <- [0..sx-1], y <- [0..sy-1]]

  rl_actions pr s =
    case Set.member s (rl_terminal_states pr) of
      True -> Set.empty
      False -> Set.fromList [minBound..maxBound]

  rl_transitions (GW (sx,sy) _) (x,y) a =
    let
      check (x',y') =
        if x' >= 0 && x' < sx && y' >= 0 && y' < sy then
          (x',y')
        else
          (x,y)
    in
    Set.fromList [(
        case a of
           L -> check (x-1,y)
           R -> check (x+1,y)
           U -> check (x,y-1)
           D -> check (x,y+1)
        , 1%1)]

  rl_reward (GW (sx,sy) _) s a s' = -1

  rl_terminal_states (GW _ exits) = exits


instance (Fractional num, Ord num) => DP_Policy num GWRandomPolicy GW (Int,Int) Action where

  rlp_action GWRandomPolicy g s = (\x -> (x,1%(toInteger $ length a)))`Set.map`a
    where
      a = rl_actions g s


gw_eval_dp :: (Fractional num, Ord num, Real num) => GW num -> IO (StateVal num (Int,Int))
gw_eval_dp gw =
  let
    opts = defaultOpts{eo_max_iter=300, eo_gamma = 1, eo_etha = 0.001}
  in do
  v <- policy_eval gw GWRandomPolicy opts (zero_sate_values gw)
  showStateVal gw v
  p' <- policy_improve gw opts v
  showGenericPolicy gw p'
  return v



