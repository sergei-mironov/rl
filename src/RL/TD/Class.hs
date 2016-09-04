module RL.TD.Class where

import qualified Data.HashMap.Strict as HashMap
import qualified Control.Monad.Rnd as Rnd

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
  => TD_Number -> pr -> [(a,TD_Number)] -> m (a,TD_Number)
eps_action eps pr as = do
  let (abest, qbest) = maximumBy (compare`on`snd) as
  let arest = filter (\x -> fst x /= abest) as
  join $ Rnd.fromList [
    swap (toRational (1.0-eps), do
      return (td_mark_best pr True abest, qbest)),
    swap (toRational eps, do
      (r,q) <- Rnd.uniform arest
      return (td_mark_best pr False r, q))
    ]


class (TD_Problem pr s a) => TD_Driver pr m s a | pr -> m where
  td_trace :: (MonadRnd g m) => pr -> s -> a -> Q s a -> m ()


-- FIXME: Move to Types.hs
type Q s a = HashMap s (HashMap a TD_Number)

emptyQ :: Q s a
emptyQ = HashMap.empty

type V s = HashMap s TD_Number

q2v :: Q s a -> V s
q2v = HashMap.map (snd . maximumBy (compare`on`snd) . HashMap.toList)

-- FIXME: handle missing states case
diffV :: (Eq s, Hashable s) => V s -> V s -> TD_Number
diffV tgt src = sum (HashMap.intersectionWith (\a b -> abs (a - b)) tgt src)

