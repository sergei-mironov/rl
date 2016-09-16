module RL.Utils where

import qualified Control.Monad.Rnd as Rnd

import RL.Imports

-- | Return @eps@-greedy action for some state of problem @pr@. The state is
-- described with assosiated list of weighted actions @as@
eps_greedy_action :: (Fractional num, Ord num, Real num, Eq a, MonadRnd g m)
  => num -> (Bool -> a -> a) -> [(a,num)] -> m (a,num)
eps_greedy_action eps greedy as = do
  let (abest, qbest) = maximumBy (compare`on`snd) as
  let arest = filter (\x -> fst x /= abest) as
  join $ Rnd.fromList [
    swap (toRational (1.0-eps), do
      -- traceM "greedy"
      return (greedy True abest, qbest)),
    swap (toRational eps, do
      -- traceM "random"
      (r,q) <- Rnd.uniform arest
      return (greedy False r, q))
    ]
