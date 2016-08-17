{-# LANGUAGE DeriveFunctor, DeriveAnyClass #-}
module RL.Q.Interp where

import qualified Data.List as List
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashMap.Binary as HashMap
import qualified Data.HashSet as HashSet
import qualified Control.Lens as Lens

import Control.Monad.Trans.Free.Church

import RL.Imports
import RL.Q_Free

type Q s a = HashMap s [(a,Q_Number)]

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

qexec' :: (MonadRnd g (Q_AlgT s a m), Q_Problem s a) => Q_Opts -> (Q_AlgT s a) m s
qexec' = qexec

runAlg :: forall g m s a . (MonadRnd g m, Q_Problem s a) => s -> Q_AlgT s a m s -> m s
runAlg s0 f = iterT go f where
  go :: Q_AlgF s a (m x) -> m x
  go (InitialState next) = next s0


-- test :: (MonadRnd g m, Q_Problem s a) => s -> m s
-- test s0 = runAlg s0 (improveT $ qexec defaultOpts)
