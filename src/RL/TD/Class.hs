module RL.TD.Class where

import RL.Imports

type TD_Number = Double

class (Eq a, Eq s, Hashable a, Hashable s, Bounded a, Enum a, Show a) =>
        TD_Problem pr s a | pr -> s , pr -> a where
  q_reward :: pr -> s -> a -> s -> TD_Number
  q_is_terminal :: pr -> s -> Bool
  q_mark_best :: pr -> Bool -> a -> a
  q_transition :: pr -> s -> a -> s

