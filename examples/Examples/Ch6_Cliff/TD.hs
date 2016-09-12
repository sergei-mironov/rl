module Examples.Ch6_Cliff.TD (cw_iter_q) where

import qualified Prelude
import qualified Data.HashMap.Strict as HashMap

import RL.Imports
import RL.TD.Types as TD
import RL.TD as TD

import Examples.Ch6_Cliff.Rules(CW(..), Point, Action)
import qualified Examples.Ch6_Cliff.Rules as Rules

data TD_CW m = TD_CW {
    cw :: CW
  , cw_trace :: Point -> Action -> Q Point Action -> m ()
  }

instance (Monad m) => TD_Problem (TD_CW m) m Point Action where
  td_is_terminal TD_CW{..} p = (p == Rules.exits cw)
  td_greedy TD_CW{..} best = id
  td_reward TD_CW{..} = Rules.reward cw
  td_transition TD_CW{..} s a q = return (fst $ Rules.transition cw s a)
  td_modify TD_CW{..} s a q = cw_trace s a q

showV gw v = Rules.showV gw (HashMap.toList v)

cw_iter_q :: CW -> IO ()
cw_iter_q cw =
  let
    -- Q options
    o = Q_Opts {
           o_alpha = 0.1
         , o_gamma = 1.0
         , o_eps = 0.7
         }

    q0 = TD.emptyQ 0   -- Initial Q table
    g0 = pureMT 33     -- Initial RNG
    cnt = 20*10^3

    st_q :: Lens' (a,b) a
    st_q = _1
    st_i :: Lens' (a,b) b
    st_i = _2
  in do

  flip evalRndT_ g0 $ do
    flip execStateT (q0,0) $ do
      loop $ do
        s0 <- Rules.pickState cw
        i <- use st_i
        q <- use st_q

        (s',q') <-
          q_learn o q s0 $ TD_CW cw $ \s a q -> do
            i <- use st_i
            when (i >= cnt) $ do
              break ()

        liftIO $ putStrLn $ "Loop i = " <> show i
        liftIO $ showV cw (TD.toV q')
        st_i %= const (i+1)
        st_q %= const q'

