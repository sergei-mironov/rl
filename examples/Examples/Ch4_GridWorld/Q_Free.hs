module Examples.Ch4_GridWorld.Q_Free where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set

import RL.Types
import RL.Imports
import RL.Q_Free

import Examples.Ch4_GridWorld.Base


instance Q_Action Action where
  q_mark_best _ = id

-- instance Q_State Point where
--   q_is_final Point =
