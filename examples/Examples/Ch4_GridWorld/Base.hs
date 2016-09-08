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

showGW :: (MonadIO m, Real num) => GW num -> (Point -> Maybe num) -> m ()
showGW (GW (sx,sy) _) lookup = liftIO $ do
  forM_ [0..sy-1] $ \y -> do
    forM_ [0..sx-1] $ \x -> do
      case lookup (x,y) of
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

-- FIXME: remove recursion
arbitraryState :: MonadRnd g m => GW t -> m Point
arbitraryState gw@GW{..} = do
    let (sx,sy) = gw_size
    x <- getRndR (0,sx-1)
    y <- getRndR (0,sy-1)
    case (x,y) `member` gw_exits of
      True -> arbitraryState gw
      False -> return (x,y)


transition :: GW num -> Point -> Action -> Point
transition (GW (sx,sy) exits) (x,y) a =
  let
    check (x',y') =
      if x' >= 0 && x' < sx && y' >= 0 && y' < sy then
        (x',y')
      else
        (x,y)

  in
  case a of
    L -> check (x-1,y)
    R -> check (x+1,y)
    U -> check (x,y-1)
    D -> check (x,y+1)

isTerminal GW{..} p = p `Set.member` gw_exits

withLearnPlot cnt f = do
  d <- newData "learnRate"
  withPlot "plot1" [heredoc|
    set grid back ls 102
    set xrange [0:${show cnt}]
    set yrange [-20:20]
    set terminal x11 1 noraise
    done = 0
    bind all 'd' 'done = 1'
    while(!done) {
      plot ${dat d} using 1:2 with lines
      pause 1
    }
    |] (f d)

