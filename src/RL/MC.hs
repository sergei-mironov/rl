{-# LANGUAGE DeriveFunctor #-}
module RL.MC where

import qualified Data.HashMap.Strict as HashMap
import qualified Prelude

import RL.Imports
import RL.Types

data MC_Opts = MC_Opts {
    o_alpha :: MC_Number
  , o_maxlen :: Int
  , o_maxlen_reward :: MC_Number
} deriving (Show)

defaultOpts = MC_Opts {
    o_alpha = 0.1
  , o_maxlen = 1000
  , o_maxlen_reward = -100.0
  }

type MC_Number = Double
type Q s a = M s a MC_Number
type V s = HashMap s MC_Number

emptyQ :: MC_Number -> Q s a
emptyQ = initM

q2v :: (Bounded a, Enum a, Eq a, Hashable a, Eq s, Hashable s) => Q s a -> V s
q2v = foldMap_s (\(s,l) -> HashMap.singleton s (snd $ layer_s_max l))

-- FIXME: handle missing states case
diffV :: (Eq s, Hashable s) => V s -> V s -> MC_Number
diffV tgt src = sum (HashMap.intersectionWith (\a b -> abs ((a) - (b))) tgt src)

toV :: (Bounded a, Enum a, Eq a, Hashable a, Eq s, Hashable s) => Q s a -> V s
toV = foldMap_s (\(s,l) -> HashMap.singleton s (snd $ layer_s_max l))

class (Fractional num, Ord s, Ord a, Show s, Show a, Bounded a, Enum a) =>
    MC_Problem pr s a num | pr->s, pr->a, pr->num where
  mc_is_terminal :: pr -> s -> Bool
  mc_reward :: pr -> s -> a -> s -> num

queryQ s = HashMap.toList <$> get_s s <$> get
modifyQ s a f = modify (modify_s_a s a f)

data MC pr m s a = MC {
    mc_pr :: pr
  , mc_transition :: s -> a -> m s
}

-- | MC-ES learning algorithm, pg 5.4. Alpha-learing rate is used instead of
-- total averaging, maximum episode length is limited to make sure policy it
-- terminates
mc_es_learn :: (Monad m, Hashable s, Hashable a, MC_Problem pr s a MC_Number)
  => MC_Opts -> Q s a -> s -> a -> MC pr m s a -> m (Q s a)
mc_es_learn MC_Opts{..} q0 s0 a0 mc@(MC pr transition) = do
  flip execStateT q0 $ do

    {- Build an episode -}
    ep <- do
      view _3 <$> do
      loopM (s0,a0,[],True) (view _4) $ \(s,a,ep,_) -> do
        s' <- lift $ mc_transition mc s a
        a' <- fst . maximumBy (compare`on`snd) <$> queryQ s'
        if length ep > o_maxlen then
          return (s', a', (s,a,s',o_maxlen_reward):ep, False)
        else do
          r <- pure $ mc_reward pr s a s'
          if mc_is_terminal pr s' then
            return (s', a', (s,a,s',r):ep, False)
          else do
            return (s', a', (s,a,s',r):ep, True)

    {- Build first-visit revard map -}
    rm <- do
      fst <$> do
      flip execStateT (mempty, 0) $ do
      forM ep $ \(s,a,s',r) -> do
        modify $ \(m,g) -> (HashMap.insert (s,a) (g+r) m, g+r)

    {- Update Q -}
    forM_ (HashMap.toList rm) $ \((s,a),g) -> do
      modifyQ s a $ \q -> q + o_alpha*(g - q)


