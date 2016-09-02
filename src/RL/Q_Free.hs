{-# LANGUAGE DeriveFunctor, DeriveAnyClass #-}
module RL.Q_Free (
    module RL.Q_Free
  , module RL.Q.Alg
  ) where

import qualified Data.List as List
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashMap.Binary as HashMap
import qualified Data.HashSet as HashSet
import qualified Control.Lens as Lens

import Control.Monad.Trans.Free.Church

import RL.Imports
import RL.Q.Alg

type Q s a = HashMap s (HashMap a Q_Number)

emptyQ :: Q s a
emptyQ = HashMap.empty

type V s = HashMap s Q_Number

q2v :: Q s a -> V s
q2v = HashMap.map (snd . maximumBy (compare`on`snd) . HashMap.toList)

-- FIXME: handle missing states case
diffV :: (Eq s, Hashable s) => V s -> V s -> Q_Number
diffV tgt src = sum (HashMap.intersectionWith (\a b -> abs (a - b)) tgt src)


class (Q_Problem pr s a) => Q_Driver pr m s a | pr -> m where
  q_trace :: (MonadRnd g m) => pr -> s -> a -> Q s a -> m ()

runAlg :: forall pr s a m g . (Show s, Q_Driver pr m s a, MonadRnd g m)
  => (pr -> FT (Q_AlgF s a) (StateT (Q s a) m) s)
  -> s
  -> Q_Number
  -> Q s a
  -> pr
  -> m (Q s a)
runAlg alg s0 qsa0 q0 pr = flip execStateT q0 $ iterT go (alg pr) where

  qs0 :: HashMap a Q_Number
  qs0 = HashMap.fromList [(a,qsa0) | a <- [minBound .. maxBound]]


  getDef d s m =
    case HashMap.lookup s m of
      Just x -> x`HashMap.union`d
      Nothing -> d

  getDef2 d s m =
    case HashMap.lookup s m of
      Just x -> x
      Nothing -> d

  go :: Q_AlgF s a (StateT (Q s a) m s) -> StateT (Q s a) m s
  go (InitialState next) = next s0
  go (Get_Actions s next) = do
    get >>= \q ->
      case HashMap.lookup s q of
        Nothing -> next (HashMap.toList qs0)
        Just qs -> next (HashMap.toList qs)
  go (Modify_Q s a f next) = do

    saq <- get
    -- traceM saq
    aq <- pure $ getDef qs0 s saq
    q <- pure $ getDef2 qsa0 a aq
    aq' <- pure $ HashMap.insert a (f q) aq
    saq' <- pure $ HashMap.insert s aq' saq
    put saq'
    -- traceM saq'
    lift (q_trace pr s a saq')
    next

    -- let qs0' = HashMap.insert a (f qsa0) qs0
    -- get >>= traceM
    -- modify $ HashMap.insertWith (const . HashMap.insertWith (const . f) a qsa0) s qs0'
    -- get >>= traceM
    -- get >>= lift . q_trace pr s a
    -- lift (q_trace pr s a saq')
    -- next


qexec o = runAlg (qexecF o)
qlearn o = runAlg (qlearnF o)

-- test :: (MonadRnd g m, Q_Problem s a) => s -> m s
-- test s0 = runAlg s0 (qexec defaultOpts)

