{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Ch_4_Gambler where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import qualified Data.List as List
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Ratio
import Text.Printf
import Debug.Trace

import Types as RL
import DP as RL

data Bet = Bet {
    bet_amount :: Int
  }
  deriving(Show, Eq, Ord)

data Game num = Game {
    game_win_score :: Int
  }
  deriving(Show)

data Gambler = Gambler {
    g_pocket :: Int
  }
  deriving(Show, Eq, Ord)


instance (Fractional num, Ord num) => DP_Problem num Game Gambler Bet where

  rl_states Game{..} =
    Set.fromList [Gambler pocket | pocket <- [0..game_win_score]]

  rl_actions Game{..} Gambler{..} =
    if g_pocket >= game_win_score || g_pocket <= 0 then
      Set.empty
    else
      let
        border = g_pocket `min` (game_win_score - g_pocket)
      in
      Set.fromList [Bet x | x <- [1..border]]

  rl_transitions Game{..} Gambler{..} Bet{..} =
      Set.fromList [
          (Gambler (g_pocket - bet_amount), 6%10)
        , (Gambler (g_pocket + bet_amount), 4%10)]

  rl_reward Game{..} _ _ Gambler{..} =
      if g_pocket >= game_win_score then
        1
      else
        0

  rl_terminal_states Game{..} = Set.fromList [ Gambler 0, Gambler game_win_score ]

thegame :: Game Rational
thegame = Game 100


example_4_3 :: forall num . (Fractional num, Ord num, Real num) => Game num -> IO ()
example_4_3 thegame =
  let
    showValPolicy :: (StateVal num Gambler, GenericPolicy Gambler Bet) -> String
    showValPolicy (v@StateVal{..}, GenericPolicy{..}) =
      unlines $
      flip map (Map.toAscList v_map `zip` Map.toAscList _p_map) $ \((_, vs) , (s@Gambler{..}, set)) ->
        let
          actmap = List.sortOn (\(p,a)-> a) $ Set.toList set

          show_actmap =
            let
              d :: Bet -> Double
              d a = fromRational $ toRational $ stateval v s a
            in
            List.intercalate "," $
            flip map actmap $ \(a@Bet{..},p) -> show bet_amount ++ " (" ++ (printf "%2.5f" (d a)) ++ ")"

          (Bet{..}, mx)
            | null actmap = (Bet 0, 0)
            | otherwise = head $ actmap
          show_vs = printf "% 2.5f" (fromRational $ toRational vs :: Double)
        in
        (printf ("%02d: ") g_pocket) ++ (replicate bet_amount '#') ++ show bet_amount ++ " " ++ show_vs ++ "  " ++ show_actmap

    debugState :: (StateVal num Gambler, GenericPolicy Gambler Bet) -> IO ()
    debugState = putStrLn . showValPolicy

    opts :: EvalOpts num s a
    opts = defaultOpts {
        eo_max_iter = 1
      , eo_gamma = 1.0
      , eo_etha = 0.00001
      -- , eo_debug = const $ return ()
    }

    stateval :: (DP_Problem num Game s a) => StateVal num s -> s-> a -> num
    stateval v s a = policy_action_value thegame s a opts v

  in do
  (v,p) <- policy_iteraton thegame (uniformGenericPolicy thegame) (zero_sate_values thegame) opts
  debugState (v,p)

