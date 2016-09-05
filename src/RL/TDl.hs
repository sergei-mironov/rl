{-# LANGUAGE DeriveFunctor #-}
module RL.TDl where

import qualified Data.List as List
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

import Control.Monad.Trans.Free.Church
import RL.Imports
import RL.TD.Class

-- data TDl_AlgF s a next =
--     InitialState (s -> next)
--   | Query_Q s ([(a,TD_Number)] -> next)
--   | Query_Z ([(s,a,TD_Number)] -> next)
--   | Modify_Q s a (TD_Number -> TD_Number) next
--   | Modify_Z s a (TD_Number -> TD_Number) next
--   deriving(Functor)

-- makeFree ''TDl_AlgF

data TDl_Opts = TDl_Opts {
    o_alpha :: TD_Number
  , o_gamma :: TD_Number
  , o_eps :: TD_Number
  } deriving (Show)

type Q s a = HashMap s (HashMap a TD_Number)
type Z s a = HashMap s (HashMap a TD_Number)

data TDl_State s a = TDl_State {
    tdl_q :: Q s a
  , tdl_z :: Z s a
  }

initialState :: TDl_State s a
initialState = undefined

getQ :: (MonadState (TDl_State s a) m) => s -> a -> m TD_Number
getQ s a = undefined

queryQ :: (MonadState (TDl_State s a) m) => s -> m [(a,TD_Number)]
queryQ s = undefined

modifyQ :: (MonadState (TDl_State s a) m) => s -> a -> (TD_Number -> TD_Number) -> m ()
modifyQ s a f = undefined


getZ :: (MonadState (TDl_State s a) m) => s -> a -> m TD_Number
getZ s a = undefined

queryZ :: (MonadState (TDl_State s a) m) => m [(s,a,TD_Number)]
queryZ = undefined

modifyZ :: (MonadState (TDl_State s a) m) => s -> a -> (TD_Number -> TD_Number) -> m ()
modifyZ s a d = undefined

class (TD_Problem pr s a) => TDl_Driver pr m s a | pr -> m where
  td_trace :: (MonadRnd g m) => pr -> s -> a -> Q s a -> m ()

-- FIXME: make lambda-related logic a runner task
sarsa_lambda :: (MonadRnd g m, TDl_Driver pr m s a)
  => TD_Number -> s -> TDl_Opts -> pr -> m s

sarsa_lambda lambda s0 TDl_Opts{..} pr = do
  flip evalStateT initialState $ do
    (a0,q0) <- queryQ s0 >>= eps_action o_eps pr
    fst <$> do
      return (s0,(a0,q0)) >>= do
      iterateUntilM (td_is_terminal pr . fst) $ \(s,(a,qsa)) -> do
        s' <- pure $ td_transition pr s a
        r <- pure $ td_reward pr s a s'
        (a',qs'a') <- eps_action o_eps pr =<< queryQ s'
        delta <- pure $ r + o_gamma * qs'a' - qsa
        modifyZ s a (+1)
        zs <- queryZ
        forM_ zs $ \(s,a,z) -> do
          modifyQ s a (\q -> q + o_alpha * delta * z)
          modifyZ s a (const $ o_gamma * lambda * z)
        return (s',(a',qs'a'))



-- -- FIXME: re-implement Get-Actions case more carefully
-- runAlg :: forall pr s a m g . (Show s, TD_Driver pr m s a, MonadRnd g m)
--   => (pr -> FT (TDl_AlgF s a) (StateT (Q s a) m) s)
--   -> s
--   -> TD_Number
--   -> Q s a
--   -> pr
--   -> m (Q s a)
-- runAlg alg s0 qsa0 q0 pr = flip execStateT q0 $ iterT go (alg pr) where

--   qs0 :: HashMap a TD_Number
--   qs0 = HashMap.fromList [(a,qsa0) | a <- [minBound .. maxBound]]

--   lookupAQ d s m =
--     case HashMap.lookup s m of
--       Just x -> x`HashMap.union`d
--       Nothing -> d

--   lookupQ d s m =
--     case HashMap.lookup s m of
--       Just x -> x
--       Nothing -> d

--   go :: TDl_AlgF s a (StateT (Q s a) m s) -> StateT (Q s a) m s
--   go = \case

--     InitialState next -> do
--       next s0

--     Query_Q s next -> do
--       get >>= \q ->
--         case HashMap.lookup s q of
--           Nothing -> next (HashMap.toList qs0)
--           Just qs -> next (HashMap.toList qs)

--     Modify_Q s a f next -> do
--       saq <- get
--       -- traceM saq
--       aq <- pure $ lookupAQ qs0 s saq
--       q <- pure $ lookupQ qsa0 a aq
--       aq' <- pure $ HashMap.insert a (f q) aq
--       saq' <- pure $ HashMap.insert s aq' saq
--       put saq'
--       -- traceM saq'
--       lift (td_trace pr s a saq')
--       next

--     Query_Z next -> do
--       undefined

--     Modify_Z s a f next -> do
--       undefined


