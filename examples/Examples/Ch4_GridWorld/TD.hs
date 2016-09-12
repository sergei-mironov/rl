module Examples.Ch4_GridWorld.TD (gw_iter_q) where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Prelude

import RL.Types hiding (Q, q2v)
import RL.Imports
import RL.TD.Types as TD
import RL.TD as TD
import RL.DP as DP

import Examples.Ch4_GridWorld.Rules(GW(..), Point, Action)
import qualified Examples.Ch4_GridWorld.Rules as Rules
import qualified Examples.Ch4_GridWorld.DP as DP

data TD_GW m = TD_GW {
    gw :: GW TD_Number
  , gw_trace :: Point -> Action -> Q Point Action -> m ()
  }

instance (Monad m) => TD_Problem (TD_GW m) m Point Action where
  td_is_terminal TD_GW{..} p = Rules.isTerminal gw p
  td_greedy TD_GW{..} best = id
  td_reward TD_GW{..} s a s' = -1
  td_transition TD_GW{..} s a q = return (Rules.transition gw s a)
  td_modify TD_GW{..} s a q = gw_trace s a q

showV gw v = Rules.showV gw (HashMap.toList v)

gw_iter_q :: GW TD_Number -> IO ()
gw_iter_q gw =
  let
    -- Q options
    o = Q_Opts {
           o_alpha = 0.1
         , o_gamma = 1.0
         , o_eps = 0.3
         }

    q0 = TD.emptyQ 0   -- Initial Q table
    g0 = pureMT 33     -- Initial RNG
    cnt = 20*10^3

    st_q :: Lens' (a,b) a
    st_q = _1
    st_i :: Lens' (a,b) b
    st_i = _2
  in do

  {- Reference StateVal -}
  (v_dp, p_dp) <- DP.gw_iter_dp gw

  Rules.withLearnPlot cnt $ \d -> do
    flip evalRndT_ g0 $ do
      flip execStateT (q0,0) $ do
        loop $ do
          s0 <- Rules.arbitraryState gw
          i <- use st_i
          q <- use st_q

          (s',q') <-
            q_learn o q s0 $ TD_GW gw $ \s a q -> do
              i <- use st_i
              when (i >= cnt) $ do
                break ()

          liftIO $ putStrLn $ "Loop i = " <> show i
          liftIO $ showV gw (TD.toV q')
          liftIO $ pushData d i (DP.diffV (TD.toV q') v_dp)
          st_i %= (+1)
          st_q %= const q'

