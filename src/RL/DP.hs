{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
module RL.DP where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Prelude hiding(break)

import RL.Imports
import RL.Types as RL

{-
  ____ _
 / ___| | __ _ ___ ___  ___  ___
| |   | |/ _` / __/ __|/ _ \/ __|
| |___| | (_| \__ \__ \  __/\__ \
 \____|_|\__,_|___/___/\___||___/
-}

zero_sate_values :: (DP_Problem num pr s a)
  => pr num -> StateVal num s
zero_sate_values pr =  StateVal $ Map.fromList $ map (\s -> (s,0.0)) (Set.toList $ rl_states pr)

-- FIXME: Convert to fold-like style eventially
-- | Dynamic Programming Problem. Parameters have the following meaning: @num@ -
-- Type of Numbers; @pr@ - the problem; @s@ - State; @a@ - Action
class (Ord s, Fractional num, Ord num) => DP_Problem num pr s a | pr -> s , pr -> a where
  rl_states :: pr num -> Set s
  rl_actions :: pr num -> s -> Set a
  rl_transitions :: pr num -> s -> a -> Set (s, Probability)
  rl_reward :: pr num -> s -> a -> s -> num
  -- FIXME: think about splitting terminal and non-terminal states
  rl_terminal_states :: pr num -> Set s

-- | Dynamic Programming Policy. Parameters have same meaning as in DP_Problem,
-- @p@ means the Policy
class (DP_Problem num pr s a) => DP_Policy num p pr s a where
  rlp_action :: p -> pr num -> s -> Set (a,Probability)

-- | For given state, probabilities for all possible action should sum up to 1
invariant_probable_actions :: (DP_Problem num pr s a, Show s, Show a) => pr num -> Bool
invariant_probable_actions pr =
  flip all (rl_states pr) $ \s ->
    flip all (rl_actions pr s) $ \a ->
      case sum (map snd (Set.toList (rl_transitions pr s a))) of
        1 -> True
        x -> error $ "Total probability of state " ++ show s ++ " action " ++ show a ++ " sum up to " ++ show x

-- | No action leads to unlisted state
invariant_closed_transition :: (DP_Problem num pr s a, Show s, Show a) => pr num -> Bool
invariant_closed_transition pr =
  flip all (rl_states pr) $ \s ->
    flip all (rl_actions pr s) $ \a ->
      flip all (rl_transitions pr s a) $ \(s',p) ->
        case (Set.member s' (rl_states pr)) of
          True -> True
          False -> error $ "State " ++ show s ++ ", action " ++ show a ++ " lead to invalid state " ++ show s'

-- | Terminal states are dead ends and non-terminal states are not
invariant_no_dead_states :: (DP_Problem num pr s a, Show s, Show a) => pr num -> Bool
invariant_no_dead_states pr =
  flip all (rl_states pr) $ \s ->
    case (member s (rl_terminal_states pr), Set.null (rl_actions pr s)) of
      (True,True) -> True
      (True,False) -> error $ "Terminal state " ++ show s ++ " is not dead end"
      (False,False) -> True
      (False,True) -> error $ "State " ++ show s ++ " is dead end"

-- Terminals are valid states
invariant_terminal :: (DP_Problem num pr s a, Show s, Show a) => pr num -> Bool
invariant_terminal pr =
  flip all (rl_terminal_states pr) $ \st ->
    case Set.member st (rl_states pr) of
      True -> True
      False -> error $ "State " ++ show st ++ " is not a valid state"

-- Policy returns valid actions
invariant_policy_actions :: (DP_Policy num p pr s a, Ord a, Show s, Show a) => p -> pr num -> Bool
invariant_policy_actions p pr =
  flip all (rl_states pr) $ \s ->
    flip all (rlp_action p pr s) $ \(a, prob) ->
      case Set.member a (rl_actions pr s) of
        True -> True
        False -> error $ "Policy from state " ++ show s ++ " leads to invalid action " ++ show a

-- Policy return valid probabilities
invariant_policy_prob :: (DP_Policy num p pr s a, Ord a, Show s, Show a) => p -> pr num -> Bool
invariant_policy_prob p pr =
  flip all (rl_states pr) $ \s ->
    let
      as = Set.toList (rlp_action p pr s)
    in
    case sum $ map snd as of
      1 -> True
      0 | null as -> True
      x -> error $ "Policy state " ++ show s ++ " probabilities sum up to " ++ show x

invariant :: (DP_Problem num pr s a, Show s, Show a, Ord a) => pr num -> Bool
invariant pr = all ($ pr) [
    invariant_probable_actions
  , invariant_closed_transition
  , invariant_terminal
  , invariant_policy_actions (uniformGenericPolicy pr)
  , invariant_policy_prob (uniformGenericPolicy pr)
  , invariant_no_dead_states
  ]

policy_eq :: (Eq a, DP_Policy num p1 pr s a, DP_Policy num p2 pr s a) => pr num -> p1 -> p2 -> Bool
policy_eq pr p1 p2 = all (\s -> (rlp_action p1 pr s) == (rlp_action p2 pr s)) (rl_states pr)

instance (DP_Problem num pr s a) => DP_Policy num (GenericPolicy s a) pr s a where
  rlp_action GenericPolicy{..} _ s = _p_map ! s

uniformGenericPolicy :: (Ord a, DP_Problem num pr s a) => pr num -> GenericPolicy s a
uniformGenericPolicy pr = GenericPolicy{..} where
  _p_map = Map.fromList $ flip map (Set.toList (rl_states pr)) $ \s ->
    let
      as = rl_actions pr s
    in
    (s, Set.map (\a -> (a, 1%(toInteger $ length as))) as)



{-
    _    _
   / \  | | __ _
  / _ \ | |/ _` |
 / ___ \| | (_| |
/_/   \_\_|\__, |
           |___/
-}


data EvalOpts num s a = EvalOpts {
    eo_gamma :: num
  -- ^ Forgetness
  , eo_etha :: num
  -- ^ policy evaluation precision
  , eo_max_iter :: Int
  -- ^ policy evaluation iteration limit, [1..maxBound]
  -- , eo_floating_precision :: Double

  , eo_debug :: (StateVal num s, GenericPolicy s a) -> IO ()
  } --deriving(Show)

defaultOpts :: (Fractional num) => EvalOpts num s a
defaultOpts = EvalOpts {
    eo_gamma = 0.9
  , eo_etha = 0.1
  , eo_max_iter = 10^3
  -- , eo_floating_precision = 1/10^9
  , eo_debug = error "no debug specified"
  }

data EvalState num s = EvalState {
    _es_delta :: num
  , _es_v :: Map s num
  , _es_v' :: Map s num
  , _es_iter :: Int
  } deriving(Show)

makeLenses ''EvalState

initEvalState :: (Fractional num) => StateVal num s -> EvalState num s
initEvalState StateVal{..} = EvalState 0 v_map v_map 0

-- | Iterative policy evaluation algorithm
-- Figure 4.1, pg.86.
policy_eval :: (DP_Policy num p pr s a, MonadIO m)
  => pr num -> p -> EvalOpts num s a -> StateVal num s -> m (StateVal num s)
policy_eval pr p EvalOpts{..} v = do
  let sum l f = List.sum <$> forM (Set.toList l) f

  StateVal . view es_v <$> do
    flip execStateT (initEvalState v) $ loop $ do

      i <- use es_iter
      when (i > eo_max_iter-1) $ do
        break ()

      es_delta %= const 0

      forM_ (rl_states pr) $ \s -> do
        v_s <- uses es_v (!s)
        v's <- do
          sum (rlp_action p pr s) $ \(a, fromRational -> pa) -> do
            (pa*) <$> do
              sum (rl_transitions pr s a) $ \(s', fromRational -> p) -> do
                v_s' <- uses es_v (!s')
                pure $ p * ((rl_reward pr s a s') + eo_gamma * (v_s'))

        es_v' %= (Map.insert s v's)
        es_delta %= (`max`(abs (v's - v_s)))

      d <- use es_delta
      when (d < eo_etha) $ do
        break ()

      v' <- use es_v'
      es_v %= const v'

      es_iter %= (+1)

policy_action_value :: (DP_Problem num pr s a) => pr num -> s  -> a -> EvalOpts num s a -> StateVal num s -> num
policy_action_value pr s a EvalOpts{..} StateVal{..} =
  List.sum $
  flip map (Set.toList $ rl_transitions pr s a) $ \(s', fromRational -> p) ->
    p * ((rl_reward pr s a s') + eo_gamma * (v_map ! s'))

policy_improve :: (DP_Problem num pr s a, MonadIO m, Ord a)
  => pr num -> EvalOpts num s a -> StateVal num s -> m (GenericPolicy s a)
policy_improve pr eo@EvalOpts{..} v@StateVal{..} = do
  let sum l f = List.sum <$> forM (Set.toList l) f

  GenericPolicy <$> do
    flip execStateT Map.empty $ do

      forM_ (rl_states pr) $ \s -> do
        (maxv, maxa) <- do
          foldlM (\(val,maxa) a -> do
                    pi_s <- pure $ policy_action_value pr s a eo v
                    return $
                      if Set.null maxa then
                        (pi_s, Set.singleton a)
                      else
                        if pi_s > val then
                          -- GT
                          (pi_s, Set.singleton a)
                        else
                          if pi_s < val then
                            --LT
                            (val,maxa)
                          else
                            -- EQ
                            (val, Set.insert a maxa)
                 ) (0 ,Set.empty) (rl_actions pr s)

        let nmax = toInteger (Set.size maxa)
        modify $ Map.insert s (Set.map (\a -> (a,1%nmax)) maxa)


policy_iteraton_step :: (DP_Policy num p pr s a, MonadIO m, Ord a)
  => pr num -> p -> StateVal num s -> EvalOpts num s a -> m (StateVal num s, GenericPolicy s a)
policy_iteraton_step pr p v eo = do
  v' <- policy_eval pr p eo v
  p' <- policy_improve pr eo v'
  return (v',p')

data PolicyContainer num p s a = APolicy p | GPolicy (GenericPolicy s a)

withAnyPolicy :: (DP_Policy num p pr s a, Monad m)
  => pr num -> PolicyContainer num p s a -> (forall p1 . DP_Policy num p1 pr s a => p1 -> m x) -> m x
withAnyPolicy pr ap handler = do
  case ap of
    APolicy p -> handler p
    GPolicy gp -> handler gp

policy_iteraton :: (DP_Policy num p pr s a, MonadIO m, Ord a)
  => pr num -> p -> StateVal num s -> EvalOpts num s a -> m (StateVal num s, GenericPolicy s a)
policy_iteraton pr p v eo = do
  (v', GPolicy p') <- flip execStateT (v, APolicy p) $ loop $ do
    (v,ap) <- get
    withAnyPolicy pr ap $ \p -> do
        (v', p') <- policy_iteraton_step pr p v eo
        liftIO $ eo_debug eo (v', p')
        put (v', GPolicy p')
        when (policy_eq pr p p') $ do
          break ()
  return (v',p')



