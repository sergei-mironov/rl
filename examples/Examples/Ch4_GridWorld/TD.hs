module Examples.Ch4_GridWorld.TD where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Prelude

import RL.Types hiding (Q, q2v)
import RL.Imports
import RL.TD as TD

import Examples.Ch4_GridWorld.Base(GW(..), Point, Action)
import qualified Examples.Ch4_GridWorld.Base as GW
import qualified Examples.Ch4_GridWorld.DP as DP

data Q_GW m = Q_GW {
    gw_base :: GW Q_Number
  , gw_trace :: Point -> Action -> Q Point Action -> m ()
  }

instance Q_Problem (Q_GW m) Point Action where
  q_reward Q_GW{..} s1 a s2 = -1
  q_is_terminal Q_GW{..} p = p `Set.member` (gw_exits gw_base)
  q_mark_best Q_GW{..} best = id
  q_transition Q_GW{..} s a = fst $ GW.transition gw_base s a

instance Q_Driver (Q_GW m) m Point Action where
  q_trace = gw_trace

sv2v :: (Hashable s, Eq s) => StateVal Q_Number s -> V s
sv2v sv = HashMap.fromList $ Map.toList $ v_map sv

showStateVal gw v = GW.showGW gw (\p -> HashMap.lookup p v)

gw_iter_q :: GW Q_Number -> IO ()
gw_iter_q gw =
  let
    -- Q options
    o = Q_Opts {
           o_alpha = 0.1
         , o_gamma = 1.0
         , o_eps = 0.7
         }

    q0 = TD.emptyQ    -- Initial Q table
    g0 = pureMT 33   -- Initial RNG
    cnt = 200*10^3

    st_q :: Lens' (a,b) a
    st_q = _1
    st_i :: Lens' (a,b) b
    st_i = _2
  in do

  {- Reference StateVal -}
  v_dp <- DP.gw_iter_dp gw

  GW.withLearnPlot cnt $ \d -> do
    flip evalRndT_ g0 $ do
      flip execStateT (q0,0) $ do
        loop $ do
          s0 <- GW.arbitraryState gw
          q <- use st_q
          qlearn o s0 0 q $ Q_GW gw $ \s a q -> do
            liftIO $ putStrLn $ show s <> "  " <> GW.showAction a
            i <- use st_i
            when (i >= cnt) $ do
              liftIO $ putStrLn $ "Exiting at " <> show i
              break ()
            st_q %= const q
            st_i %= (+1)
          (q',i) <- get
          liftIO $ putStrLn $ "Loop i = " <> show i
          liftIO $ showStateVal gw (q2v q')
          liftIO $ pushData d i (diffV (q2v q') (sv2v v_dp))
          st_q %= const q'

