module Examples.Ch4_GridWorld (
    module Examples.Ch4_GridWorld
  ) where


import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import RL.Types
import RL.Imports
import RL.DP as DP
import RL.TD as TD
import RL.TDl as TDl

import Examples.Ch4_GridWorld.Rules as Rules
import Examples.Ch4_GridWorld.DP as DP
-- import Examples.Ch4_GridWorld.MC as MC
import Examples.Ch4_GridWorld.TD as TD
import Examples.Ch4_GridWorld.TDl as TDl

gw :: GW Rational
gw = GW (4,4) (Set.fromList [(0,0),(3,3)])

gw_d :: GW Double
gw_d = GW (4,4) (Set.fromList [(0,0),(3,3)])

gw2 :: GW num
gw2 = GW (2,1) (Set.fromList [(1,0)])


data S = S {
    st_i :: Integer
  , st_q :: TD.Q Point Action
  , st_tdl :: TDl.Q Point Action
  , st_qlw :: TDl.Q Point Action
  }


gw_iter_all :: GW Double -> IO ()
gw_iter_all gw =
  let
    {- Number of iterations -}
    cnt = 2000
    {- Epsilon-greedy policy -}
    eps = 0.01
    {- Learning rate -}
    alpha = 0.1

    oq = Q_Opts {
           o_alpha = alpha
         , o_gamma = 1.0
         , o_eps = eps
         }

    otdl = TDl_Opts {
           o_alpha = alpha
         , o_gamma = 1.0
         , o_eps = eps
         , o_lambda = 0.8
         }

    oqlw = otdl

    q0 = TD.emptyQ 0
    tdl0 = TDl.emptyQ 0
    qlw0 = TDl.emptyQ 0

    g0 = pureMT 33

  in do
  (v_dp, p_dp) <- DP.gw_iter_dp gw

  dq <- newData "q"
  dtdl <- newData "tdl"
  dqlw <- newData "qlw"

  withPlot "plot1" [heredoc|
    set grid back ls 102
    set xrange [0:${show cnt}]
    set yrange [-20:20]
    set terminal x11 1 noraise
    done = 0
    bind all 'd' 'done = 1'
    while(!done) {
      plot ${dat dq} using 1:2 with lines, ${dat dtdl} using 1:2 with lines, ${dat dqlw} using 1:2 with lines
      pause 1
    }
    |] $ do

    flip evalRndT_ g0 $ do
    flip execStateT (S 0 q0 tdl0 qlw0) $ do
    loop $ do
      s0 <- Rules.arbitraryState gw
      s@S{..} <- get

      (_, q') <- do
        q_learn oq st_q s0 $ TD_GW gw $ \s a q -> return ()

      (_, tdl') <- do
        tdl_learn otdl st_tdl s0 $ TDl_GW gw $ \s a q -> return ()

      (_, qlw') <- do
        qlw_learn oqlw st_qlw s0 $ TDl_GW gw $ \s a q -> return ()

      liftIO $ putStrLn $ "Loop i = " <> show st_i
      liftIO $ pushData dq (fromInteger st_i) (DP.diffV (TD.toV q') v_dp)
      liftIO $ pushData dtdl (fromInteger st_i) (DP.diffV (TD.toV tdl') v_dp)
      liftIO $ pushData dqlw (fromInteger st_i) (DP.diffV (TD.toV qlw') v_dp)

      put s{st_i = st_i + 1, st_q = q' , st_tdl = tdl', st_qlw = qlw' }

