{-# LANGUAGE DeriveFunctor, DeriveAnyClass #-}
module RL.Q_Free where

import qualified Data.List as List
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashMap.Binary as HashMap
import qualified Data.HashSet as HashSet
import qualified Control.Lens as Lens
import Control.Monad.Rnd as Rnd
import Control.Monad.Free
import Control.Monad.Free.TH (makeFree)
import Prelude hiding (break)

import RL.Imports

type Q_Number = Double

data Q_Policy = Q_Policy {
    _p_eps :: Q_Number
  } deriving(Show,Read)

$(makeLenses ''Q_Policy)

data Q s a = Q {
    _q_map :: ! (HashMap s (HashMap a Q_Number))
  } deriving(Show,Read,Generic,Binary)

$(makeLenses ''RL.Q_Free.Q)

emptyQ :: Q s a
emptyQ = Q HashMap.empty

q2v :: (Eq s, Hashable s, Eq a, Hashable a) => Q s a -> s -> Q_Number
q2v q s = foldl' max 0 (q^.q_map.(zidx mempty s))

zidx def name = Lens.lens get set where
  get m = case HashMap.lookup name m of
            Just x -> x
            Nothing -> def
  set = (\hs mhv -> HashMap.insert name mhv hs)

newtype FinalState = FinalState Bool
  deriving(Show, Eq)

newtype Reward = Reward Q_Number

data Q_AlgF s a next =
    InitialState (s -> next)
  | Transition s (a,Bool) ((s,FinalState,Reward) -> next)
  | QueryQ s (HashMap a Q_Number -> next)
  | Fin
  deriving(Functor)

makeFree ''Q_AlgF

type Q_Alg s a = Free (Q_AlgF s a)

data Q_State s sr a = Q_State {
    _s_q :: Q sr a
  -- ^ Q-table
  , _s_s :: s
  -- ^ Final state
  }

$(makeLenses ''Q_State)

data Q_Opts = Q_Opts {
    _q_alpha :: Q_Number
  , _q_gamma :: Q_Number
  , _q_eps :: Q_Number
} deriving (Show)

defaultOpts = Q_Opts {
    _q_alpha = 0.1
  , _q_gamma = 0.5
  , _q_eps = 0.3
  }

$(makeLenses ''Q_Opts)


class (Enum a, Bounded a, Eq a, Hashable a, Eq s, Hashable s) => Q_Problem s a


qaction :: (MonadRnd g m, Q_Problem s a) => Q_Opts -> Q s a -> s -> m (a,Bool)
qaction o q s =
  let
    {- Stored actions -}
    amap = fromMaybe HashMap.empty $ HashMap.lookup s (q^.q_map)

    {- Map of weighted available actions
     - Use small positive reward to make the agent more qurious
     -}
    qacts =
      flip map [minBound..maxBound] $ \a ->
        let q = fromMaybe 0.01 $ HashMap.lookup a amap
        in (a,q)

    {- Best action -}
    abest = fst $ maximumBy (compare`on`snd) qacts

    {- Rest available actions -}
    arest = map fst $ filter (\x -> fst x /= abest) qacts

  in

  join $ Rnd.fromList [
    swap (toRational $ 1.0-(o^.q_eps), do
      return (abest,True)),
    swap (toRational $ o^.q_eps, do
      r <- Rnd.uniform arest
      return (r,False))
    ]

qexec :: (MonadRnd g m, Q_Problem s a, MonadFree (Q_AlgF s a) m) => Q_Opts -> Q s a -> m ()
qexec o q0 = do
  s0 <- initialState
  flip evalStateT (Q_State q0 s0) $ do
  forever $ do
    s <- use s_s
    q <- use s_q
    ab <- qaction o q s
    (s', isfin, reward) <- transition s ab
    when (isfin == FinalState True) $ do
      fin




