module Examples.Ch4_GridWorld.Q_Free where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set

import RL.Types
import RL.Imports
import RL.Q_Free as Q

import Examples.Ch4_GridWorld.Base(GW(..), Point, Action)
import qualified Examples.Ch4_GridWorld.Base as GW

data Q_GW m = Q_GW {
    gw :: GW Q_Number
  , qtrans :: Point -> Action -> m Point
  }

instance Q_Problem (Q_GW m) m Point Action where
  q_reward Q_GW{..} s1 a s2 = -1
  q_is_terminal Q_GW{..} p = p `Set.member` (gw_exits gw)
  q_mark_best Q_GW{..} best = id
  q_transition Q_GW{..} = qtrans

gw_iter_q :: GW Q_Number -> IO ()
gw_iter_q gw =
  let
    qo = defaultOpts
    q0 = Q.emptyQ
    g0 = pureMT 33
    s0 :: Point
    s0 = (0,0)
  in do
  flip runRndT g0 $ do
    flip execStateT True $ do
      qlearn qo s0 0 q0 $ Q_GW gw $ \s a -> do
        put False
        liftIO $ putStrLn "transition!"
        return $ fst $ GW.transition gw s a
  return ()

