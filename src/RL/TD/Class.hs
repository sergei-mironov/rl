module RL.TD.Class where

import Control.Monad.Rnd as Rnd
import RL.Imports

type TD_Number = Double

class (Eq a, Eq s, Hashable a, Hashable s, Bounded a, Enum a, Show a) =>
        TD_Problem pr s a | pr -> s , pr -> a where
  td_reward :: pr -> s -> a -> s -> TD_Number
  td_is_terminal :: pr -> s -> Bool
  td_mark_best :: pr -> Bool -> a -> a
  td_transition :: pr -> s -> a -> s

-- | Return @eps@-greedy action for some state of problem @pr@. The state is
-- described with assosiated list of weighted actions @as@
eps_action :: (MonadRnd g m, TD_Problem pr s a)
  => TD_Number -> pr -> [(a,TD_Number)] -> m a
eps_action eps pr as = do
  let abest = fst $ maximumBy (compare`on`snd) as
  let arest = map fst $ filter (\x -> fst x /= abest) as
  join $ Rnd.fromList [
    swap (toRational (1.0-eps), do
      return (td_mark_best pr True abest)),
    swap (toRational eps, do
      r <- Rnd.uniform arest
      return (td_mark_best pr False r))
    ]

