module RL.TD.Class where

import qualified Data.HashMap.Strict as HashMap
import qualified Control.Monad.Rnd as Rnd

import RL.Imports
import RL.TD.Types

class (Eq s, Hashable s, Show s) => TD_State s
class (Eq a, Hashable a, Enum a, Bounded a, Show a) => TD_Action a

-- class (TD_State s, TD_Action a) => TD_Problem pr s a | pr -> s , pr -> a where
--   td_reward :: pr -> s -> a -> s -> TD_Number
--   td_is_terminal :: pr -> s -> Bool
--   td_mark_best :: pr -> Bool -> a -> a
--   td_transition :: pr -> s -> a -> s

-- | Return @eps@-greedy action for some state of problem @pr@. The state is
-- described with assosiated list of weighted actions @as@
eps_greedy_action :: (Eq a, MonadRnd g m)
  => TD_Number -> (Bool -> a -> a) -> [(a,TD_Number)] -> m (a,TD_Number)
eps_greedy_action eps greedy as = do
  let (abest, qbest) = maximumBy (compare`on`snd) as
  let arest = filter (\x -> fst x /= abest) as
  join $ Rnd.fromList [
    swap (toRational (1.0-eps), do
      return (greedy False abest, qbest)),
    swap (toRational eps, do
      (r,q) <- Rnd.uniform arest
      return (greedy True r, q))
    ]



