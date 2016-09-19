{-# LANGUAGE QuasiQuotes #-}
module Examples.Ch4_GridWorld.MC where

import qualified Data.List as List
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
-- import qualified Control.Monad. as Set

import Control.Monad.Except

import RL.Types
import RL.Imports
import RL.MC as MC

import Examples.Ch4_GridWorld.Rules(GW(..), Point, Action)
import qualified Examples.Ch4_GridWorld.Rules as Rules
import qualified Examples.Ch4_GridWorld.DP as DP

instance (Fractional num, Real num, Ord num) => MC_Problem (GW num) Point Action num where
  mc_is_terminal = Rules.isTerminal
  mc_reward gw s a s' = -1

-- | Uniform random policy iteration with MC alg
gw_iter_mc :: GW MC_Number -> IO ()
gw_iter_mc gw =
  let

    o = MC_Opts {
      o_alpha = 0.1
    }

    cnt = 20*10^3
    g0 = pureMT 33     -- Initial RNG
    q0 = MC.emptyQ 0   -- Initial Q table

    st_q :: Lens' (a,b,c) a
    st_q = _1
    st_i :: Lens' (a,b,c) b
    st_i = _2
    st_len :: Lens' (a,b,c) c
    st_len = _3

  in do
  {- Reference StateVal -}
  (v_dp, p_dp) <- DP.gw_iter_dp gw

  Rules.withLearnPlot cnt $ \d ->

    flip evalRndT_ g0 $ do
    flip execStateT (q0,0,0) $ do
    loop $ do

      s0 <- Rules.arbitraryState gw
      a0 <- uniform [minBound..maxBound]

      i <- use st_i
      q <- use st_q
      st_len %= const 0

      mq <-
        runExceptT $ do
          mc_es_learn o q s0 a0 $ MC gw $ \s a -> do
            st_len %= (+1)
            l <- use st_len
            when (l > 100) $ do
              throwError $ "Episode is too long"
            return $ Rules.transition gw s a

      case mq of

        Right q' -> do
          liftIO $ putStrLn $ "Loop i = " <> show i
          liftIO $ Rules.showV gw (HashMap.toList $ MC.toV q')
          liftIO $ pushData d i (MC.diffV (MC.toV q') v_dp)
          st_i %= (+1)
          st_q %= const q'

        Left err -> do
          -- liftIO $ putStrLn $ "Skipping episode due to error: " <> err
          return ()

