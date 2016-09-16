module RL.Types where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

import RL.Imports

type Layer a num = HashMap a num
type Storage s a num = HashMap s (Layer a num)

-- | Base container used in most of RL algorithms. @M x0 sto@ describes the
-- 2-dimentional array (`Storage` of `Layers`) where each layer containes fixed
-- number of elements. New layers are filled with the range of
-- @[minBound..maxBound]@ default values @x0@
data M s a num = M {
    x0 :: num
  , sto :: Storage s a num
  } deriving(Show)

-- | Initialises new container, set default layer value to @x@
initM :: num -> M s a num
initM x = M x HashMap.empty

mmod :: (Storage s a num -> Storage s a num) -> M s a num -> M s a num
mmod f m = m { sto = f (sto m) }

aq0 :: (Eq a, Enum a, Hashable a, Bounded a)
  => num -> HashMap a num
aq0 q0 = HashMap.fromList [(a,q0) | a <- [minBound .. maxBound]]

get_s :: (Eq a, Enum a, Hashable a, Bounded a, Eq s, Hashable s)
  => s -> M s a num -> Layer a num
get_s s (M x0 sto) = maybe (aq0 x0) (`HashMap.union` (aq0 x0)) . HashMap.lookup s $ sto

layer_s_max :: (Eq a, Enum a, Hashable a, Bounded a, Ord num)
  => Layer a num -> (a,num)
layer_s_max = maximumBy (compare`on`snd) . HashMap.toList

get_s_a :: (Eq a, Enum a, Hashable a, Bounded a, Eq s, Hashable s)
  => s -> a -> M s a num -> num
get_s_a s a (M x0 sto) = maybe x0 (maybe x0 id . HashMap.lookup a) . HashMap.lookup s $ sto

put_s :: (Eq s, Hashable s, Bounded a, Enum a, Eq a, Hashable a)
  => s -> HashMap a num -> M s a num -> M s a num
put_s s x = mmod $ HashMap.unionWith HashMap.union (HashMap.singleton s x)

put_s_a :: (Eq s, Hashable s, Bounded a, Enum a, Eq a, Hashable a)
  => s -> a -> num -> M s a num -> M s a num
put_s_a s a x = put_s s (HashMap.singleton a x)

modify_s_a :: (Eq s, Hashable s, Bounded a, Enum a, Eq a, Hashable a)
  => s -> a -> (num -> num) -> M s a num -> M s a num
modify_s_a s a f q = put_s_a s a (f (get_s_a s a q)) q

list :: M s a num -> [(s,a,num)]
list q = flip concatMap (HashMap.toList (sto q)) $ \(s,aq) -> flip map (HashMap.toList aq) $ \(a,q) -> (s,a,q)

foldMap_s :: (Eq a, Bounded a, Enum a, Hashable a, Monoid acc) => ((s,Layer a num) -> acc) -> M s a num -> acc
foldMap_s f (M x0 sto) = foldMap (f . (id *** (`HashMap.union`(aq0 x0)))) (HashMap.toList sto)

fold_s :: (Eq a, Bounded a, Enum a, Hashable a, Monoid acc) => (acc -> (s,Layer a num) -> acc) -> acc -> M s a num -> acc
fold_s f acc0 (M x0 sto) = foldl' go acc0 (HashMap.toList sto) where
  go acc (s,l) = f acc (s,l`HashMap.union`(aq0 x0))

