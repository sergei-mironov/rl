{-# LANGUAGE DeriveFunctor #-}
module RL.MC where

import qualified Data.HashMap.Strict as HashMap
import qualified Prelude

import RL.Imports
import RL.Types

data MC_Opts = MC_Opts {
    o_alpha :: MC_Number
} deriving (Show)

defaultOpts = MC_Opts {
    o_alpha = 0.1
  }

type MC_Number = Double
type Q s a = M s a MC_Number
type V s a = HashMap s (a, MC_Number)

emptyQ :: MC_Number -> Q s a
emptyQ = initM

q2v :: (Bounded a, Enum a, Eq a, Hashable a, Eq s, Hashable s) => Q s a -> V s a
q2v = foldMap_s (\(s,l) -> HashMap.singleton s (layer_s_max l))

-- FIXME: handle missing states case
diffV :: (Eq s, Hashable s) => V s a -> V s a -> MC_Number
diffV tgt src = sum (HashMap.intersectionWith (\a b -> abs ((snd a) - (snd b))) tgt src)

class (Monad m, Fractional num, Ord s, Ord a, Show s, Show a, Bounded a, Enum a) =>
    MC_Problem pr m s a num | pr->m, pr->s, pr->a, pr->num where
  mc_is_terminal :: pr -> s -> Bool
  mc_reward :: pr -> s -> a -> s -> num
  mc_transition :: pr -> s -> a -> m s

queryQ s = HashMap.toList <$> get_s s <$> get
modifyQ pr s a f = modify (modify_s_a s a f)

-- | Builds the reward map for an episode, counting only first visits.
episode :: (Hashable s, Hashable a, MC_Problem pr m s a MC_Number)
  => pr -> s -> Q s a -> m (HashMap (s,a) MC_Number)
episode pr s0 q0 = do
  {- episode -}
  ep <- do
    snd <$> do
    flip evalStateT q0 $ do
    loopM (s0,[]) (\(s,_) -> not $ mc_is_terminal pr s) $ \(s,ep) -> do
      a <- fst . maximumBy (compare`on`snd) <$> queryQ s
      s' <- lift $ mc_transition pr s a
      return (s',(s,a,s'):ep)
  {- revard map -}
  rm <- do
    fst <$> do
    flip execStateT (mempty, 0) $ do
    forM ep $ \(s,a,s') -> do
      r <- pure $ mc_reward pr s a s'
      modify $ \(m,g) -> (HashMap.insert (s,a) (g+r) m, g+r)
  return rm

-- | MC-ES learning algorithm, pg 5.4. Alpha-learing rate is used instead of
-- total averaging
mc_es_learn :: (Hashable s, Hashable a, MC_Problem pr m s a MC_Number)
  => MC_Opts -> Q s a -> s -> pr -> m (Q s a)
mc_es_learn MC_Opts{..} q0 s0 pr = do
  rm <- episode pr s0 q0
  flip execStateT q0 $ do
    forM_ (HashMap.toList rm) $ \((s,a),g) -> do
      modifyQ pr s a $ \q -> q + o_alpha*(g - q)


