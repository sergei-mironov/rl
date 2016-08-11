{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Examples.Ch4_GridWorld.Base where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import RL.Types
import RL.Imports
import RL.DP

{-
  ____      _     _                    _     _
 / ___|_ __(_) __| |_      _____  _ __| | __| |
| |  _| '__| |/ _` \ \ /\ / / _ \| '__| |/ _` |
| |_| | |  | | (_| |\ V  V / (_) | |  | | (_| |
 \____|_|  |_|\__,_| \_/\_/ \___/|_|  |_|\__,_|

 Example 4.1, pg.86
-}


type Point = (Int,Int)

data Action = L | R | U | D
  deriving(Show, Eq, Ord, Enum, Bounded, Generic, Hashable)

showAction :: Action -> String
showAction a =
  case a of
    L->"<"
    R->">"
    U->"^"
    D->"v"

showActions :: Set Action -> String
showActions = concat . map showAction . List.sort . Set.toList

data GW num = GW {
    gw_size :: (Int,Int),
    gw_exits :: Set (Int,Int)
  }
  deriving(Show)

data GWRandomPolicy = GWRandomPolicy
  deriving(Show)

showStateVal :: (MonadIO m, Real num) => GW num -> StateVal num Point -> m ()
showStateVal (GW (sx,sy) _) StateVal{..} = liftIO $ do
  forM_ [0..sy-1] $ \y -> do
    forM_ [0..sx-1] $ \x -> do
      case Map.lookup (x,y) v_map of
        Just v -> do
          printf "%-2.1f " ((fromRational $ toRational v) :: Double)
        Nothing -> do
          printf "  ?   "
    printf "\n"

showGenericPolicy :: (MonadIO m, DP_Policy num (GenericPolicy Point Action) GW Point Action)
  => GW num
  -> GenericPolicy Point Action
  -> m ()
showGenericPolicy pr@(GW (sx,sy) _) p@GenericPolicy{..} = liftIO $ do
  forM_ [0..sy-1] $ \y -> do
    forM_ [0..sx-1] $ \x -> do
      case Map.lookup (x,y) (view p_map p) of
        Nothing ->  do
          printf "  ?   "
        Just ap -> do
          let acts = Set.map fst $ Set.filter (\(a,pa) -> pa > 0) ap
          printf "% 4s " (showActions acts)
    printf "\n"

