module Examples.Ch4_GridWorld.Q_Free where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set

import RL.Types hiding (Q)
import RL.Imports
import RL.Q_Free as Q

import Examples.Ch4_GridWorld.Base(GW(..), Point, Action)
import qualified Examples.Ch4_GridWorld.Base as GW

data Q_GW m = Q_GW {
    gw :: GW Q_Number
  , gw_trace :: Point -> Action -> Q Point Action -> m ()
  }

instance Q_Problem (Q_GW m) Point Action where
  q_reward Q_GW{..} s1 a s2 = -1
  q_is_terminal Q_GW{..} p = p `Set.member` (gw_exits gw)
  q_mark_best Q_GW{..} best = id
  q_transition Q_GW{..} s a = fst $ GW.transition gw s a

instance Q_Driver (Q_GW m) m Point Action where
  q_trace = gw_trace

gw_iter_q :: GW Q_Number -> IO ()
gw_iter_q gw =
  let
    qo = defaultOpts -- Q options
    q0 = Q.emptyQ -- Initial Q table
    g0 = pureMT 33 -- Initial RNG
    s0 = (0,0) -- Initial state TODO: get it random
  in do
  flip runRndT g0 $ do
    flip execStateT False $ do
      qlearn qo s0 0 q0 $ Q_GW gw $ \s a q -> do
        put True
        liftIO $ putStrLn "Q update!"
  return ()

