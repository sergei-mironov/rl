{-# LANGUAGE DeriveFunctor #-}
module RL.TDl where

import Control.Monad.Loops
import qualified Data.List as List

import RL.Imports
import RL.TD.Class

data TDl_AlgF s a next =
    InitialState (s -> next)
  | Query_Q s ([(a,TD_Number)] -> next)
  | Query_Z ([(s,a,TD_Number)] -> next)
  | Modify_Q s a (TD_Number -> TD_Number) next
  | Modify_Z s a (TD_Number -> TD_Number) next
  deriving(Functor)

makeFree ''TDl_AlgF

data TDl_Opts = TDl_Opts {
    o_alpha :: TD_Number
  , o_gamma :: TD_Number
  , o_eps :: TD_Number
  } deriving (Show)

access a = fromMaybe err . List.lookup a where
  err = error "assert: access: no such action"

sarsa_lambda :: (MonadRnd g m, TD_Problem pr s a, MonadFree (TDl_AlgF s a) m) =>
  TD_Number -> TDl_Opts -> pr -> m s
sarsa_lambda lambda TDl_Opts{..} pr = do
  s0 <- initialState
  (a0,q0) <- query_Q s0 >>= eps_action o_eps pr
  fst <$> do
    return (s0,(a0,q0)) >>= do
    iterateUntilM (td_is_terminal pr . fst) $ \(s,(a,qsa)) -> do
      s' <- pure $ td_transition pr s a
      r <- pure $ td_reward pr s a s'
      (a',qs'a') <- eps_action o_eps pr =<< query_Q s'
      delta <- pure $ r + o_gamma * qs'a' - qsa
      modify_Z s a (+1)
      zs <- query_Z
      forM_ zs $ \(s,a,z) -> do
        modify_Q s a (\q -> q + o_alpha * delta * z)
        modify_Z s a (const $ o_gamma * lambda * z)
      return (s',(a',qs'a'))




