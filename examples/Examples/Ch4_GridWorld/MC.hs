{-# LANGUAGE QuasiQuotes #-}
module Examples.Ch4_GridWorld.MC where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


import RL.Types
import RL.Imports
import RL.MC
import RL.MC.ES

import Examples.Ch4_GridWorld.Base
import Examples.Ch4_GridWorld.DP as DP


instance (Fractional num, Real num, Ord num) => MC_Problem num GW (Int,Int) Action where

  mc_state_nonterm gw@(GW (sx,sy) exits) g =
    let
      (p,g') = flip runRnd g $ do
            x <- getRndR (0,sx-1)
            y <- getRndR (0,sy-1)
            return (x,y)
    in
      -- FIXME: try to replace recursion with direct selection
      case p `member` exits of
        True -> mc_state_nonterm gw g'
        False -> (p,g')

  mc_actions pr s = Set.fromList [minBound .. maxBound]

  mc_transition gw s a g = (transition gw s a, g)

  mc_reward (GW (sx,sy) _) s a s' = -1

instance (Fractional num, Real num, Ord num) => MC_Policy num GW (Int,Int) Action GWRandomPolicy where
  mc_action pr s p =
    runRnd $ uniform [minBound .. maxBound]


instance (Fractional num, Real num, Ord num, Show num) => MC_Policy_Show num GW (Int,Int) Action GWRandomPolicy

instance (Fractional num, Real num, Ord num, Show num) => MC_Problem_Show num GW (Int,Int) Action


gw_eval_mc :: (Show num, Fractional num, Ord num, Real num) => GW num -> IO ()
gw_eval_mc gw = do

  let max = 200000

  v_dp <- gw_eval_dp gw

  d2 <- newData "mc2"

  withPlot "plot1" [heredoc|
    set grid back ls 102
    set xrange [0:${show max}]
    set yrange [-20:20]
    set terminal x11 1 noraise
    done = 0
    bind all 'd' 'done = 1'
    while(!done) {
      plot ${dat d2} using 1:2 with lines
      pause 1
    }
  |] $

    let

      opts = RL.MC.defaultEvalOpts{
               o_max_iter = max,
               o_ext = E_Ext {
                 eo_learnMonitor = Just Monitor{
                   mon_target = v_dp,
                   mon_data = d2
                 }
               }
             }

      g = pureMT 42

    in do
    (v,_) <- RL.MC.policy_eval opts gw GWRandomPolicy g
    showStateVal gw v



-- | Uniform random policy iteration with MC alg
gw_iter_mc :: (Show num, Fractional num, Ord num, Real num) => GW num -> IO ()
gw_iter_mc gw = do
  let max = 200000
  d <- newData "mc"

  withPlot "plot1" [heredoc|
    set grid back ls 102
    set xrange [0:${show max}]
    set yrange [-20:20]
    set terminal x11 1 noraise
    done = 0
    bind all 'd' 'done = 1'
    while(!done) {
      plot ${dat d} using 1:2 with lines
      pause 1
    }
  |] $

    let
      opts = (RL.MC.defaultOpts $ ES_Ext {
                 eo_debug = \Episode{..} ES_State{..} -> do
                  when (0 == _ess_iter `mod` 3000) $ do
                    showStateVal gw (q2v _ess_q)
                    showGenericPolicy gw _ess_p
             }) {
               o_max_iter = max
             }

      s = RL.MC.ES.initialState emptyQ emptyGenericPolicy

      g = pureMT 42

    in do

    (s',g') <- RL.MC.ES.policy_iteraton gw opts s g
    return ()

