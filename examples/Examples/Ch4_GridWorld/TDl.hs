module Examples.Ch4_GridWorld.TDl (
    gw_iter_tdl
  , gw_iter_qlw
  , TDl_GW(..)
  ) where

import qualified Prelude
import qualified Data.HashMap.Strict as HashMap

import RL.Imports
import RL.TD.Types
import RL.TDl as TDl

import Examples.Ch4_GridWorld.Rules (GW(..), Point, Action)
import qualified Examples.Ch4_GridWorld.Rules as Rules

data TDl_GW m = TDl_GW {
    gw :: GW TD_Number
  , gw_trace :: Point -> Action -> Q Point Action -> m ()
  }

instance (Monad m) => TDl_Problem (TDl_GW m) m Point Action where
  td_is_terminal TDl_GW{..} p = Rules.isTerminal gw p
  td_greedy TDl_GW{..} best = id
  td_reward TDl_GW{..} s a s' = -1
  td_transition TDl_GW{..} s a st = return (Rules.transition gw s a)
  td_modify TDl_GW{..} s a st = gw_trace s a (st^.tdl_q)

showV gw v = Rules.showV gw (HashMap.toList v)

gw_iter_tdl :: GW TD_Number -> IO ()
gw_iter_tdl gw =
  let
    o = TDl_Opts {
           o_alpha = 0.1
         , o_gamma = 1.0
         , o_eps = 0.3
         , o_lambda = 0.8
         }

    q0 = TDl.emptyQ 0   -- Initial Q table
    g0 = pureMT 33     -- Initial RNG
    cnt = 20*10^3 :: Integer

    st_q :: Lens' (a,b) a
    st_q = _1
    st_i :: Lens' (a,b) b
    st_i = _2
  in do

  flip evalRndT_ g0 $ do
    flip execStateT (q0,0) $ do
      loop $ do
        s0 <- Rules.arbitraryState gw
        i <- use st_i
        q <- use st_q

        (s',q') <-
          tdl_learn o q s0 $ TDl_GW gw $ \s a q -> do
            i <- use st_i
            when (i >= cnt) $ do
              break ()

        liftIO $ putStrLn $ "Loop i = " <> show i
        liftIO $ showV gw (TDl.toV q')
        st_i %= const (i+1)
        st_q %= const q'


gw_iter_qlw :: GW TD_Number -> IO ()
gw_iter_qlw gw =
  let
    o = TDl_Opts {
           o_alpha = 0.1
         , o_gamma = 1.0
         , o_eps = 0.3
         , o_lambda = 0.5
         }

    q0 = TDl.emptyQ 0   -- Initial Q table
    g0 = pureMT 33     -- Initial RNG
    cnt = 20*10^3 :: Integer

    st_q :: Lens' (a,b) a
    st_q = _1
    st_i :: Lens' (a,b) b
    st_i = _2
  in do

  flip evalRndT_ g0 $ do
    flip execStateT (q0,0) $ do
      loop $ do
        s0 <- Rules.arbitraryState gw
        i <- use st_i
        q <- use st_q

        (s',q') <-
          qlw_learn o q s0 $ TDl_GW gw $ \s a q -> do
            i <- use st_i
            when (i >= cnt) $ do
              break ()

        liftIO $ putStrLn $ "Loop i = " <> show i
        liftIO $ showV gw (TDl.toV q')
        st_i %= const (i+1)
        st_q %= const q'


