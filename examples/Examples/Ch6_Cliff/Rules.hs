{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Examples.Ch6_Cliff.Rules where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import RL.Imports
import RL.TD.Types

type Point = (Int,Int)

data Action = L | R | U | D
  deriving(Show, Eq, Ord, Enum, Bounded, Generic, Hashable)

showAction :: Action -> String
showAction = \case { L->"<" ; R->">" ; U->"^" ; D->"v" ; }

-- showActions :: Set Action -> String
-- showActions = concat . map showAction . List.sort . Set.toList

data CW = CW {
  cw_size :: (Int,Int)
  } deriving(Show)


cw_exit (CW (sx,sy)) = (sx-1,sy-1)

cw_transition (CW (sx,sy)) (x,y) a =
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

cw_reward cw s a s' = if fall then -100 else -1 where
  (s'', fall) = cw_transition cw s a

showActionTable :: (MonadIO m) => CW -> [(Point,(Action, TD_Number))] -> m ()
showActionTable pr@(CW (sx,sy)) at = liftIO $ do
  forM_ [0..sy-1] $ \y -> do
    forM_ [0..sx-1] $ \x -> do
      case List.lookup (x,y) at of
        Nothing ->  do
          printf "  ?   "
        Just (a,_) -> do
          printf "% 4s " (showAction a)
    printf "\n"

pickState :: MonadRnd g m => CW -> m Point
pickState (CW (sx,sy)) = do
  uniform $ (0,sy-1) : [(x,y) | x <- [0..sx-1], y <- [0..sy-2] ]
