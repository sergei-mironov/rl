{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
module TickTackToe where

import Imports
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Prelude hiding(break)

import Util
import Monad
import Types
import MC.Types as MC
import MC.ES

data Cell = X | O | E
  deriving(Show,Read,Eq,Ord)

showCell X = "X"
showCell O = "O"
showCell E = " "

type Player = Cell

nextPlayer X = O
nextPlayer O = X
nextPlayer E = error "nextPlayer: result for E is undefined"

data Board = Board {
    bo_cells :: Map (Int,Int) Cell
  , bo_wins :: Player
  , bo_term :: Bool
} deriving(Show,Read,Ord,Eq)

type Action = (Int,Int)

board_ny = 3
board_nx = 3
(.+) (a,b) (c,d) = (a+c,b+d)
(.*) x (a,b) = (x*a,x*b)
board_inside (x,y) = x >=0 && x < board_nx && y >=0 && y < board_ny
board_winsize = 3
board_points = [(x,y) | x<-[0..board_nx-1], y<-[0..board_ny-1]]
board_dirs = [(0,1),(1,0),(1,1),(1,-1)]
board_rows = filter ( and . map board_inside ) $ [ map (\x -> p .+ (x .* d)) [0..board_winsize-1] | p <- board_points, d <- board_dirs]
board_wincheck = Map.fromList [ (p,filter (p`elem`) board_rows) | p <- board_points ]

at :: Board -> Action -> Cell
at Board{..} a = fromMaybe E (Map.lookup a bo_cells)

set a c Board{..} = Board (Map.insert a c bo_cells)

move :: Board -> Player -> Action -> Board
move b@Board{..} p a =
  case bo_term of
    False ->
      let
        bo_cells' = Map.insert a p bo_cells
        bo_last' = boardFree b == [a]
        bo_winner' = or $ flip map (board_wincheck ! a) $ \r {-row-} ->
                      all (==p) (flip map r $ (\ a -> fromMaybe E $ Map.lookup a bo_cells'))
      in
      Board bo_cells' (if bo_winner' then p else E) (bo_last' || bo_winner')
    True -> error $ "move called on terminal board"

emptyBoard :: Board
emptyBoard = Board Map.empty E False

boardFree :: Board -> [Action]
boardFree Board{..} = board_points \\ (Map.keys bo_cells)

-- FIXME: eliminate potantional forever loop
randomBoard :: (RandomGen g) => Player -> g -> (Board, g)
randomBoard p g =
  let
    check ((_,b,True),g') = (b,g')
    check ((_,_,False),g') =
      -- trace "randomBoard diverged" $
      randomBoard p g'
  in do
  check $
    flip runRnd g $ do
    flip execStateT (X, emptyBoard, True) $ do
    loop $ do
      nmoves <- (
        case p of
          X -> Monad.uniform $ filter even [0 .. board_nx*board_ny-1]
          O -> Monad.uniform $ filter odd [0 .. board_nx*board_ny-1])
      forM_ [1..nmoves] $ \m -> do
        player <- use _1
        board <- use _2
        let b's = filter (not . bo_term) $ map (move board player) (boardFree board)
        case null b's of
          True -> do
            _3 %= const False
            break ()
          False -> do
            b' <- Monad.uniform b's
            _1 %= nextPlayer
            _2 %= const b'
      break ()

showBoard :: (MonadIO m) => Board -> m ()
showBoard b =
  liftIO $ do
  putStrLn ".---."
  forM_ [0..board_ny-1] $ \y -> do
    putStr "|"
    forM_ [0..board_nx-1] $ \x -> do
      putStr $ showCell (b `at` (x,y))
    putStrLn "|"
  putStrLn ".---."

showRandomBoard :: _ -> IO ()
showRandomBoard seed = showBoard $ fst $ randomBoard X (pureMT seed)

showEpisode :: (Show num, MonadIO m, MC_Problem num T Board Action) => T num -> Episode Board Action -> m ()
showEpisode pr@T{..} e = do
  let es = episode_forward e
  showBoard $ view _1 $ head es
  forM_ es $ \(b,a,b') -> do
    liftIO $ putStrLn $ show a
    showBoard b'
  let (b,a,b') = head (episode_backward e)
  liftIO $ putStrLn $ "Player " ++ show t_player ++ " R " ++ show (mc_reward pr b a b')

-- | TickTackToe => T
data T num = T {
    t_vals :: Map Player (Q num Board Action)
  , t_player :: Player
} deriving(Show,Read)

bestAction :: (Fractional num, Ord num, RandomGen g)
  => T num -> Board -> Player -> g -> Maybe (Action, g)
bestAction T{..} b p g =
  let
    macts = Map.lookup b $ view q_map $ t_vals ! p
  in
  case macts of
    Just as -> Just (fst $ maximumBy (compare `on` (current . snd)) (Map.toList as), g)
    Nothing ->
      case boardFree b of
        [] -> Nothing
        x -> Just $ flip runRnd g $ Monad.uniform x


instance (Fractional num, Real num, Ord num) => MC_Problem num T Board Action where

  mc_state_nonterm T{..} = randomBoard t_player

  mc_actions T{..} = Set.fromList . boardFree

  mc_transition t@T{..} b a g =
    let
      {- Applying action -}
      b' = move b t_player a
    in
    case bo_term b' of
      True ->
        {- Win -}
        ((b', True), g)
      False ->
        let
          p' = nextPlayer t_player
        in
        case bestAction t b' p' g of
          Nothing ->
            {- Draw -}
            ((b', True), g)
          Just (a',g') ->
            {- Make opponent's move according to their current state-value -}
            let
              b'' = move b' p' a'
            in
            {- Next|Loose -}
            ((b'', bo_term b''), g')

  mc_reward T{..} b a b' =
    if bo_term b' then
      if | bo_wins b' == t_player -> 1
         | bo_wins b' == E -> 0
         | otherwise -> -1
    else
      0


instance (Fractional num, Real num, Ord num, Show num) => MC_Problem_Show num T Board Action

t0 = T {
    t_vals = Map.fromList [
        (X,emptyQ),
        (O,emptyQ)
      ],
    t_player = O
  }


t1 = T {
    t_vals = Map.fromList [
        (X,emptyQ),
        (O,emptyQ)
      ],
    t_player = X
  }


example :: (Show num, Fractional num, Ord num, Real num) => T num -> IO ()
example pr = do

  flip evalStateT ((0,0,0),pr) $ do

  loop $ do

    pr@T{..} <- use _2

    _1 %= const (0,0,0)

    let
      g = pureMT 33

      o = (MC.defaultOpts $ ES_Ext {
            eo_debug = \e@Episode{..} ES_State{..} -> do
              let winner = bo_wins (episodeFinal e)
              if | winner == t_player -> do
                  _1 . _1 %= (+1)
                 | winner == E -> do
                  _1 . _2 %= (+1)
                 | winner == (nextPlayer t_player) -> do
                  _1 . _3 %= (+1)
              when (0 == _ess_iter `mod` 100) $ do
                score <- use _1
                traceM (t_player, _ess_iter, score, sizeQ _ess_q)

              when (0 == _ess_iter `mod` 1000) $ do
                showEpisode pr e
          }) {
            o_max_iter = 10000
          }

      s = MC.ES.initialState (t_vals ! t_player) emptyGenericPolicy

    (s',g') <- MC.ES.policy_iteraton pr o s g

    let pr' = pr{
              t_vals = Map.insert t_player (_ess_q s') t_vals
            , t_player = nextPlayer t_player
            }

    _2 %= const pr'

    save "game" "data/TickTackToe" (show pr')


load :: forall num . (Show num, Read num, Fractional num, Ord num, Real num) => FilePath -> IO (T num)
load fp = do
  contents <- readFile fp
  return (read contents)

simulate :: (Show num, Fractional num, Ord num, Real num) => T num -> IO ()
simulate t = do
  flip evalStateT (X, emptyBoard) $ do
  loop $ do
    (player,board) <- get
    let (action, nmax) = maximumBy (compare `on` (current . snd)) $ Map.toList ((_q_map ((t_vals t) ! player)) ! board)
    let board' = move board player action
    liftIO $ do
      showBoard board
      putStrLn $ "Player is " ++ show player ++ "; Move is " ++ show action ++ " Learned " ++ show (avg_n nmax)
    when (bo_term board') $ do
      liftIO $ do
        showBoard board'
        putStrLn $ show (bo_wins board')
      break ()
    _1 %= nextPlayer
    _2 %= const board'





