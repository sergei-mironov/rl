module Examples.Ch4_GridWorld.Q where

import qualified Data.List as List
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set

import RL.Types
import RL.Imports
import RL.Q

import Examples.Ch4_GridWorld.Base
import Examples.Ch4_GridWorld.DP as DP


newtype Q_GW (m :: * -> *) = Q_GW (GW Q_Number)

instance (Monad m) => Q_Problem m (Q_GW m) Point Point Action where

  -- FIXME: remove recursion
  q_state gw@(Q_GW GW{..}) = do
    let (sx,sy) = gw_size
    x <- getRndR (0,sx-1)
    y <- getRndR (0,sy-1)
    case (x,y) `member` gw_exits of
      True -> q_state gw
      False -> return (x,y)

