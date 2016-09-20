{-|
  Satton, 'Reinforcement Learning: The Introduction', pg.145, Example 6.6: Cliff Walking
-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module Examples.Ch6_Cliff.Rules where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import RL.Imports
import RL.Types

type Point = (Int,Int)

data Action = L | R | U | D
  deriving(Show, Eq, Ord, Enum, Bounded, Generic, Hashable)

showAction :: Action -> String
showAction = \case { L->"<" ; R->">" ; U->"^" ; D->"v" ; }

data CW = CW {
  cw_size :: (Int,Int)
  } deriving(Show)


exits (CW (sx,sy)) = (sx-1,sy-1)

transition (CW (sx,sy)) (x,y) a =
  let
    check (x',y') =
      if x' >= 0 && x' < sx && y' >= 0 && y' < sy then
        if (y' == sy-1) && (x' /= 0) && (x' /= sx-1) then
          ((0,sy-1), True)
        else
          ((x',y'), False)
      else
        ((x,y), False)
  in
  case a of
    L -> check (x-1,y)
    R -> check (x+1,y)
    U -> check (x,y-1)
    D -> check (x,y+1)

reward cw s a s' = if fall then -100 else -1 where
  (s'', fall) = transition cw s a

showActionTable :: (MonadIO m, Fractional num, Real num) => CW -> [(Point,(Action, num))] -> m ()
showActionTable pr@(CW (sx,sy)) at = liftIO $ do
  forM_ [0..sy-1] $ \y -> do
    forM_ [0..sx-1] $ \x -> do
      case List.lookup (x,y) at of
        Nothing ->  do
          printf "% 4s " "?"
        Just (a,_) -> do
          printf "% 4s " (showAction a)
    printf "\n"

showV :: (MonadIO m, Real num) => CW -> [(Point, num)] -> m ()
showV (CW (sx,sy)) v = liftIO $ do
  forM_ [0..sy-1] $ \y -> do
    forM_ [0..sx-1] $ \x -> do
      case List.lookup (x,y) v of
        Just v -> do
          printf "%-2.1f " ((fromRational $ toRational v) :: Double)
        Nothing -> do
          printf "  ?   "
    printf "\n"

pickState :: MonadRnd g m => CW -> m Point
pickState (CW (sx,sy)) = do
  uniform $ (0,sy-1) : [(x,y) | x <- [0..sx-1], y <- [0..sy-2] ]

