{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
module RL.MC.Types where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Prelude hiding(break)

import RL.Imports
import RL.Types as RL
import Control.Monad.Rnd as RL
import RL.DP (DP_Problem(..), DP_Policy(..))
import qualified RL.DP as DP

{-
  ____ _
 / ___| | __ _ ___ ___  ___  ___
| |   | |/ _` / __/ __|/ _ \/ __|
| |___| | (_| \__ \__ \  __/\__ \
 \____|_|\__,_|___/___/\___||___/
-}

class (Fractional num, Ord s, Ord a, Ord num, Real num) => MC_Problem num pr s a | pr -> s , pr -> a where
  mc_state_nonterm :: (RandomGen g) => pr num -> g -> (s,g)
  mc_actions :: pr num -> s -> Set a
  mc_transition :: (RandomGen g) => pr num -> s -> a -> g -> ((s,Bool),g)
  mc_reward :: pr num -> s -> a -> s -> num

class (MC_Problem num pr s a) => MC_Policy num pr s a p where
  mc_action :: (RandomGen g) => pr num -> s -> p -> g -> (a,g)

class (MC_Problem num pr s a, Show s, Show a, Show (pr num), Show num) => MC_Problem_Show num pr s a

-- Too clumsy. Try using MC_Problem_Show instead
class (MC_Policy num pr s a p, Show s, Show a, Show (pr num), Show num, Show p) => MC_Policy_Show num pr s a p

instance MC_Problem num pr s a => MC_Policy num pr s a (GenericPolicy s a) where
  mc_action pr s p = runRnd $ do
    case Map.lookup s (view p_map p) of
      Nothing -> RL.uniform (Set.toList (mc_actions pr s))
      Just as -> RL.fromList (Set.toList as)

{-
-- FIXME: arrange state terminality concepts between MC and DP
--
-- DP compatibility adapter
data MC a s pr num = MC (pr num)
  deriving(Show)

instance DP_Problem num pr s a => MC_Problem num (MC a s pr) s a where
  mc_state (MC pr) = runRnd $ RL.uniform (Set.toList (rl_states pr))
  mc_actions (MC pr) = rl_actions pr
  mc_transition (MC pr) s a = runRnd $ RL.fromList $ Set.toList $ rl_transitions pr s a
  mc_reward (MC pr) = rl_reward pr
  mc_is_terminal (MC pr) s = member s (rl_terminal_states pr)

instance (DP_Problem num pr s a, DP_Policy num p pr s a) => MC_Policy num (MC a s pr) s a p where
  mc_action (MC pr) s p =
    case member s (rl_terminal_states pr) of
      True -> error "mc_action(2): attempt to query terminate state"
      False -> runRnd $ RL.fromList $ Set.toList $ rlp_action p pr s

instance (DP_Policy num p pr s a, Show num, Show a, Show s, Show p, Show (pr num)) => MC_Policy_Show num (MC a s pr) s a p
-}

{-
 _____
|_   _|   _ _ __   ___  ___
  | || | | | '_ \ / _ \/ __|
  | || |_| | |_) |  __/\__ \
  |_| \__, | .__/ \___||___/
      |___/|_|
-}

newtype Episode s a = Episode {
  ep_list :: [(s,a,s)]
  } deriving(Show)

episode_forward Episode{..} = reverse ep_list
episode_backward Episode{..} = ep_list

episodeFinal :: Episode s a -> s
episodeFinal = view _3 . head . ep_list


data Opts num ext = Opts {
    o_gamma :: num
  -- ^ Forgetness
  , o_num_prec :: num
  -- ^ policy evaluation precision
  , o_max_iter :: Integer
  -- ^ policy evaluation iteration limit, [1..maxBound]
  -- , o_floating_precision :: Double
  -- , o_debug :: Maybe ((StateVal num s, GenericPolicy s a) -> IO ())
  -- , o_learnMonitor :: Maybe (Monitor num s)
  -- , o_policyMonitor :: Maybe PlotData
  , o_maxEpisodeLen :: Integer
  , o_ext :: ext
  }

defaultOpts :: (Fractional num) => ext -> Opts num ext
defaultOpts defExt = Opts {
    o_gamma = 0.9
  , o_num_prec = 0 {- OK for Rationals -}
  , o_max_iter = 10^3
  -- , o_floating_precision = 1/10^9
  -- , o_debug = Nothing
  -- , o_learnMonitor = Nothing
  -- , o_policyMonitor = Nothing
  , o_maxEpisodeLen = 100
  , o_ext = defExt
  }



