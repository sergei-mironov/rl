
module Examples.Ch4_GridWorld where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import RL.Types
import RL.Imports

import Examples.Ch4_GridWorld.Base
import Examples.Ch4_GridWorld.DP
-- import Examples.Ch4_GridWorld.MC
import Examples.Ch4_GridWorld.TD

gw :: GW Rational
gw = GW (4,4) (Set.fromList [(0,0),(3,3)])

gw_d :: GW Double
gw_d = GW (4,4) (Set.fromList [(0,0),(3,3)])

gw2 :: GW num
gw2 = GW (2,1) (Set.fromList [(1,0)])


