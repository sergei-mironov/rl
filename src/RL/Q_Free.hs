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

-- q2v :: (Eq s, Hashable s, Eq a, Hashable a) => Q s a -> s -> Q_Number
-- q2v q s = foldl' max 0 (q^.q_map.(zidx mempty s))

-- zidx def name = Lens.lens get set where
--   get m = case HashMap.lookup name m of
--             Just x -> x
--             Nothing -> def
--   set = (\hs mhv -> HashMap.insert name mhv hs)
-- type Q_Alg s a = Free (Q_AlgF s a)

type Q_AlgT s a m = FT (Q_AlgF s a) m

-- qexec' :: (MonadRnd g (Q_AlgT s a m), Q_Problem s a) => Q_Opts -> (Q_AlgT s a) m s
-- qexec' = qexec

-- class (Q_Problem pr s a) => Q_Driver pr m s a | pr -> m where
--   q_transition :: (MonadRnd g m) => pr -> s -> a -> m s


runAlg :: (Q_Problem pr (FT (Q_AlgF s a) (StateT (Q s a) m)) s a, MonadRnd g m)
  => (pr -> FT (Q_AlgF s a) (StateT (Q s a) m) s)
  -> s
  -> Q_Number
  -> Q s a
  -> pr
  -> m (Q s a)
runAlg alg s0 qsa0 q0 pr = flip execStateT q0 $ iterT go (alg pr) where

  qs0 = HashMap.fromList [(a,qsa0) | a <- [minBound .. maxBound]]

  go (InitialState next) = next s0
  -- go (Transition s a next) = q_transition pr s a >>= next
  go (Get_Actions s next) = do
    get >>= \q ->
      case HashMap.lookup s q of
        Nothing -> next (HashMap.toList qs0)
        Just qs -> next (HashMap.toList qs)
  go (Modify_Q s a f next) = do
    let qs0' = HashMap.insert a (f qsa0) qs0
    modify $ HashMap.insertWith (const . HashMap.insertWith (const . f) a qsa0) s qs0'
    next


qexec o = runAlg (qexecF o)
qlearn o = runAlg (qlearnF o)

-- test :: (MonadRnd g m, Q_Problem s a) => s -> m s
-- test s0 = runAlg s0 (qexec defaultOpts)

