{-# LANGUAGE DeriveFunctor #-}
module RL.TD.Alg where

import Control.Monad.Rnd as Rnd
import Control.Monad.Loops
import Control.Monad.Free.Class
import Control.Monad.Free.TH (makeFree)

import RL.Imports

type TD_Number = Double

class (Eq a, Eq s, Hashable a, Hashable s, Bounded a, Enum a, Show a) =>
        TD_Problem pr s a | pr -> s , pr -> a where
  q_reward :: pr -> s -> a -> s -> TD_Number
  q_is_terminal :: pr -> s -> Bool
  q_mark_best :: pr -> Bool -> a -> a
  q_transition :: pr -> s -> a -> s


data TD_AlgF s a next =
    InitialState (s -> next)
  -- | Transition s a (s -> next)
  | Get_Actions s ([(a,TD_Number)] -> next)
  | Modify_Q s a (TD_Number -> TD_Number) next
  deriving(Functor)

makeFree ''TD_AlgF

data Q_Opts = Q_Opts {
    o_alpha :: TD_Number
  , o_gamma :: TD_Number
  , o_eps :: TD_Number
} deriving (Show)

defaultOpts = Q_Opts {
    o_alpha = 0.1
  , o_gamma = 0.5
  , o_eps = 0.3
  }

-- | Take eps-greedy action
qaction :: (MonadRnd g m, TD_Problem pr s a, MonadFree (TD_AlgF s a) m) => TD_Number -> s -> pr -> m a
qaction eps s pr = do
  qs <- get_Actions s
  let abest = fst $ maximumBy (compare`on`snd) qs
  let arest = map fst $ filter (\x -> fst x /= abest) qs
  join $ Rnd.fromList [
    swap (toRational (1.0-eps), do
      return (q_mark_best pr True abest)),
    swap (toRational eps, do
      r <- Rnd.uniform arest
      return (q_mark_best pr False r))
    ]

-- | Execute Q-actions without learning
-- FIXME: take best action
qexecF :: (MonadRnd g m, TD_Problem pr s a, MonadFree (TD_AlgF s a) m) => Q_Opts -> pr -> m s
qexecF Q_Opts{..} pr = do
  initialState >>= do
  iterateUntilM (not . q_is_terminal pr) $ \s -> do
    a <- qaction o_eps s pr
    s' <- pure $ q_transition pr s a
    return s'

-- | Execute Q-Learning algorithm
qlearnF :: (MonadRnd g m, TD_Problem pr s a, MonadFree (TD_AlgF s a) m) => Q_Opts -> pr -> m s
qlearnF Q_Opts{..} pr = do
  initialState >>= do
  iterateUntilM (q_is_terminal pr) $ \s -> do
    a <- qaction o_eps s pr
    s' <- pure $ q_transition pr s a
    r <- pure $ q_reward pr s a s
    max_qs' <- snd . maximumBy (compare`on`snd) <$> get_Actions s'
    modify_Q s a $ \qs -> qs + o_alpha * (r + o_gamma * max_qs' - qs)
    return s'

