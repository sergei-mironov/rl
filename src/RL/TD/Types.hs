module RL.TD.Types where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

import RL.Imports

type TD_Number = Double

type Layer a = HashMap a TD_Number
type Storage s a = HashMap s (Layer a)

data M s a = M {
    x0 :: TD_Number
  , sto :: Storage s a
  } deriving(Show)

initM :: TD_Number -> M s a
initM x = M x HashMap.empty

mmod :: (Storage s a -> Storage s a) -> M s a -> M s a
mmod f m = m { sto = f (sto m) }

aq0 :: (Eq a, Enum a, Hashable a, Bounded a)
  => TD_Number -> HashMap a TD_Number
aq0 q0 = HashMap.fromList [(a,q0) | a <- [minBound .. maxBound]]

get_s :: (Eq a, Enum a, Hashable a, Bounded a, Eq s, Hashable s)
  => s -> M s a -> Layer a
get_s s (M x0 sto) = maybe (aq0 x0) (`HashMap.union` (aq0 x0)) . HashMap.lookup s $ sto

layer_s_max :: (Eq a, Enum a, Hashable a, Bounded a)
  => Layer a -> (a,TD_Number)
layer_s_max = maximumBy (compare`on`snd) . HashMap.toList

get_s_a :: (Eq a, Enum a, Hashable a, Bounded a, Eq s, Hashable s)
  => s -> a -> M s a -> TD_Number
get_s_a s a (M x0 sto) = maybe x0 (maybe x0 id . HashMap.lookup a) . HashMap.lookup s $ sto

put_s :: (Eq s, Hashable s, Bounded a, Enum a, Eq a, Hashable a)
  => s -> HashMap a TD_Number -> M s a -> M s a
put_s s x = mmod $ HashMap.unionWith HashMap.union (HashMap.singleton s x)

put_s_a :: (Eq s, Hashable s, Bounded a, Enum a, Eq a, Hashable a)
  => s -> a -> TD_Number -> M s a -> M s a
put_s_a s a x = put_s s (HashMap.singleton a x)

modify_s_a :: (Eq s, Hashable s, Bounded a, Enum a, Eq a, Hashable a)
  => s -> a -> (TD_Number -> TD_Number) -> M s a -> M s a
modify_s_a s a f q = put_s_a s a (f (get_s_a s a q)) q

list :: M s a -> [(s,a,TD_Number)]
list q = flip concatMap (HashMap.toList (sto q)) $ \(s,aq) -> flip map (HashMap.toList aq) $ \(a,q) -> (s,a,q)

foldMap_s :: (Eq a, Bounded a, Enum a, Hashable a, Monoid acc) => ((s,Layer a) -> acc) -> M s a -> acc
foldMap_s f (M x0 sto) = foldMap (f . (id *** (`HashMap.union`(aq0 x0)))) (HashMap.toList sto)

fold_s :: (Eq a, Bounded a, Enum a, Hashable a, Monoid acc) => (acc -> (s,Layer a) -> acc) -> acc -> M s a -> acc
fold_s f acc0 (M x0 sto) = foldl' go acc0 (HashMap.toList sto) where
  go acc (s,l) = f acc (s,l`HashMap.union`(aq0 x0))

