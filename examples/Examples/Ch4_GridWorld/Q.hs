module Examples.Ch4_GridWorld.Q where

import qualified Data.List as List
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set

import RL.Types
import RL.Imports
import RL.Q

import Examples.Ch4_GridWorld.Base
import Examples.Ch4_GridWorld.DP as DP


data Q_GW (m :: * -> *) = Q_GW {
    qp_gw :: GW Q_Number
  , qp_max_transitions :: Int
  }

data Q_S = Q_S {
    qs_point :: Point
  , qa_n :: Int
  }

instance (Monad m) => Q_Problem m (Q_GW m) Q_S Point Action where

  q_state (Q_GW gw _) = Q_S <$> arbitraryState gw <*> pure 0

  q_transition (Q_GW gw _) (Q_S s n) a = return (Q_S (fst $ transition gw s a) (n+1))

  q_reward pr s1 a s2 = -1

  q_is_terminal (Q_GW gw maxtr) (Q_S s ntr) = s `Set.member` (gw_exits gw) || ntr >= maxtr

  q_state_reduce pr (Q_S s x) = s



{-
gw_eval_q :: (Show num, Fractional num, Ord num, Real num) => GW num -> IO ()
gw_eval_q gw = do

  let max_transitions = 200000

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
      q0 = emptyQ
    in do
    qlearn (Q_GW gw) o q0
-}

