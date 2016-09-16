module RL.DP where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import Prelude hiding(break)

import RL.Imports

-- | Probability [0..1]
type Probability = Rational

-- | Policy
type P s a = HashMap s (Set (a,Probability))

type V s num = HashMap s num

-- FIXME: handle missing states case
diffV :: (Eq s, Hashable s, Num num) => V s num -> V s num -> num
diffV tgt src = sum (HashMap.intersectionWith (\a b -> abs (a - b)) tgt src)

-- FIXME: Convert to fold-like style eventially
-- | Dynamic Programming Problem. Parameters have the following meaning: @num@ -
-- Type of Numbers; @pr@ - the problem; @s@ - State; @a@ - Action
class (Ord s, Ord a, Fractional num, Ord num, Hashable s) =>
    DP_Problem pr s a num | pr -> s, pr -> a, pr -> num where
  dp_states :: pr -> Set s
  dp_actions :: pr -> s -> Set a
  dp_transitions :: pr -> s -> a -> Set (s, Probability)
  dp_reward :: pr -> s -> a -> s -> num
  -- FIXME: think about splitting terminal and non-terminal states
  dp_terminal_states :: pr -> Set s

action :: (DP_Problem pr s a num) => pr -> P s a -> s -> Set (a,Probability)
action pr p s = p HashMap.! s

initV :: (DP_Problem pr s a num)
  => pr -> num -> V s num
initV pr num =  HashMap.fromList $ map (\s -> (s,num)) (Set.toList $ dp_states pr)

-- | For given state, probabilities for all possible action should sum up to 1
invariant_probable_actions :: (DP_Problem pr s a num, Show s, Show a) => pr -> Bool
invariant_probable_actions pr =
  flip all (dp_states pr) $ \s ->
    flip all (dp_actions pr s) $ \a ->
      case sum (map snd (Set.toList (dp_transitions pr s a))) of
        1 -> True
        x -> error $ "Total probability of state " ++ show s ++ " action " ++ show a ++ " sum up to " ++ show x

-- | No action leads to unlisted state
invariant_closed_transition :: (DP_Problem pr s a num, Show s, Show a) => pr -> Bool
invariant_closed_transition pr =
  flip all (dp_states pr) $ \s ->
    flip all (dp_actions pr s) $ \a ->
      flip all (dp_transitions pr s a) $ \(s',p) ->
        case (Set.member s' (dp_states pr)) of
          True -> True
          False -> error $ "State " ++ show s ++ ", action " ++ show a ++ " lead to invalid state " ++ show s'

-- | Terminal states are dead ends and non-terminal states are not
invariant_no_dead_states :: (DP_Problem pr s a num, Show s, Show a) => pr -> Bool
invariant_no_dead_states pr =
  flip all (dp_states pr) $ \s ->
    case (member s (dp_terminal_states pr), Set.null (dp_actions pr s)) of
      (True,True) -> True
      (True,False) -> error $ "Terminal state " ++ show s ++ " is not dead end"
      (False,False) -> True
      (False,True) -> error $ "State " ++ show s ++ " is dead end"

-- Terminals are valid states
invariant_terminal :: (DP_Problem pr s a num, Show s, Show a) => pr -> Bool
invariant_terminal pr =
  flip all (dp_terminal_states pr) $ \st ->
    case Set.member st (dp_states pr) of
      True -> True
      False -> error $ "State " ++ show st ++ " is not a valid state"

-- Policy returns valid actions
invariant_policy_actions :: (DP_Problem pr s a num, Ord a, Show s, Show a) => P s a -> pr -> Bool
invariant_policy_actions p pr =
  flip all (dp_states pr) $ \s ->
    flip all (action pr p s) $ \(a, prob) ->
      case Set.member a (dp_actions pr s) of
        True -> True
        False -> error $ "Policy from state " ++ show s ++ " leads to invalid action " ++ show a

-- Policy return valid probabilities
invariant_policy_prob :: (DP_Problem pr s a num, Ord a, Show s, Show a) => P s a -> pr -> Bool
invariant_policy_prob p pr =
  flip all (dp_states pr) $ \s ->
    let
      as = Set.toList (action pr p s)
    in
    case sum $ map snd as of
      1 -> True
      0 | null as -> True
      x -> error $ "Policy state " ++ show s ++ " probabilities sum up to " ++ show x

invariant :: (DP_Problem pr s a num, Show s, Show a, Ord a) => pr -> Bool
invariant pr = all ($ pr) [
    invariant_probable_actions
  , invariant_closed_transition
  , invariant_terminal
  , invariant_policy_actions (uniformPolicy pr)
  , invariant_policy_prob (uniformPolicy pr)
  , invariant_no_dead_states
  ]

policy_eq :: (Eq a, DP_Problem pr s a num) => pr -> P s a -> P s a -> Bool
policy_eq pr p1 p2 = all (\s -> (action pr p1 s) == (action pr p2 s)) (dp_states pr)


uniformPolicy :: (Ord a, DP_Problem pr s a num) => pr -> P s a
uniformPolicy pr =
  HashMap.fromList $ flip map (Set.toList (dp_states pr)) $ \s ->
    let
      as = dp_actions pr s
    in
    (s, Set.map (\a -> (a, 1%(toInteger $ length as))) as)


data Opts num s a = Opts {
    eo_gamma :: num
  -- ^ Forgetness
  , eo_etha :: num
  -- ^ policy evaluation precision
  , eo_max_iter :: Int
  -- ^ policy evaluation iteration limit, [1..maxBound]
  } deriving(Show)

defaultOpts :: (Fractional num) => Opts num s a
defaultOpts = Opts {
    eo_gamma = 0.9
  , eo_etha = 0.1
  , eo_max_iter = 10^3
  }

data EvalState num s = EvalState {
    _es_delta :: num
  , _es_v :: V s num
  , _es_v' :: V s num
  , _es_iter :: Int
  } deriving(Show)

makeLenses ''EvalState

initEvalState :: (Fractional num) => V s num -> EvalState num s
initEvalState v = EvalState 0 v v 0

-- | Iterative policy evaluation algorithm
-- Figure 4.1, pg.86.
policy_eval :: (Monad m, DP_Problem pr s a num)
  => Opts num s a -> P s a -> V s num -> (DP pr m s a num) -> m (V s num)
policy_eval Opts{..} p v (DP pr _) = do
  let sum l f = List.sum <$> forM (Set.toList l) f

  view es_v <$> do
    flip execStateT (initEvalState v) $ loop $ do

      i <- use es_iter
      when (i > eo_max_iter-1) $ do
        break ()

      es_delta %= const 0

      forM_ (dp_states pr) $ \s -> do
        v_s <- (HashMap.!s) <$> use es_v
        v's <- do
          sum (action pr p s) $ \(a, fromRational -> pa) -> do
            (pa*) <$> do
              sum (dp_transitions pr s a) $ \(s', fromRational -> p) -> do
                v_s' <- (HashMap.!s') <$> use es_v
                pure $ p * ((dp_reward pr s a s') + eo_gamma * (v_s'))

        es_v' %= (HashMap.insert s v's)
        es_delta %= (`max`(abs (v's - v_s)))

      d <- use es_delta
      when (d < eo_etha) $ do
        break ()

      v' <- use es_v'
      es_v %= const v'

      es_iter %= (+1)

policy_action_value :: (DP_Problem pr s a num) => Opts num s a -> s -> a -> V s num -> pr -> num
policy_action_value Opts{..} s a v pr =
  List.sum $ flip map (Set.toList $ dp_transitions pr s a) $ \(s', fromRational -> p) ->
    p * ((dp_reward pr s a s') + eo_gamma * (v HashMap.! s'))

policy_improve :: (Monad m, DP_Problem pr s a num)
  => Opts num s a -> V s num -> DP pr m s a num -> m (P s a)
policy_improve o v (DP pr _) = do
  let sum l f = List.sum <$> forM (Set.toList l) f
  flip execStateT mempty $ do
    forM_ (dp_states pr) $ \s -> do
      (maxv, maxa) <- do
        foldlM (\(val,maxa) a -> do
                  pi_s <- pure $ policy_action_value o s a v pr
                  return $
                    if Set.null maxa then
                      (pi_s, Set.singleton a)
                    else
                      if pi_s > val then
                        -- GT
                        (pi_s, Set.singleton a)
                      else
                        if pi_s < val then
                          -- LT
                          (val,maxa)
                        else
                          -- EQ
                          (val, Set.insert a maxa)
               ) (0, Set.empty) (dp_actions pr s)

      let nmax = toInteger (Set.size maxa)
      modify $ HashMap.insert s (Set.map (\a -> (a,1%nmax)) maxa)

data DP pr m s a num = DP {
    dp_pr :: pr
  , dp_trace :: V s num -> P s a -> m ()
}


policy_iteration :: (Monad m, DP_Problem pr s a num, Ord a)
  => Opts num s a -> P s a -> V s num -> (DP pr m s a num) -> m (V s num, P s a)
policy_iteration o p v dpr@(DP pr trace) = do
  let up = lift . lift
  (v', p') <-
    flip execStateT (v, p) $ do
    loop $ do
      (v,p) <- get
      v' <- up $ policy_eval o p v dpr
      p' <- up $ policy_improve o v' dpr
      up $ trace v' p'
      put (v', p')
      when (policy_eq pr p p') $ do
        break ()
  return (v',p')



