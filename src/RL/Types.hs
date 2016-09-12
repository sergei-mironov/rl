{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FunctionalDependencies #-}
module RL.Types where

import Control.Arrow ((&&&),(***))
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Random
import Control.Monad.State.Strict
import Data.Ratio
import Data.List hiding (break)
import qualified Data.List as List
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.Random

import RL.Imports

{-
debug :: (MonadIO m) => String -> m ()
debug = liftIO . putStrLn

type Probability = Rational
type Reward = Rational

data StateVal num s = StateVal {
    v_map :: Map s num
  } deriving(Show)

data GenericPolicy s a = GenericPolicy {
    _p_map :: Map s (Set (a,Probability))
  } deriving(Eq,Ord, Show)

makeLenses ''GenericPolicy

emptyGenericPolicy :: (Ord s, Ord a) => GenericPolicy s a
emptyGenericPolicy = GenericPolicy mempty

data Histogram a = Histogram {
    hist_map :: Map a Integer
  } deriving(Show)

showHist :: (Ord a) => Histogram a -> IO ()
showHist Histogram{..} = do
  let max = fromInteger $ maximum $ map snd $ Map.toAscList hist_map
  forM_ (Map.toAscList hist_map`zip`[0..]) $ \((k,fromInteger -> v),i) -> do
    putStrLn $ show i ++ " " ++ (replicate ((v * 50)`div`max) '#')

sample :: (Ord a) => Int -> (StdGen -> (a,StdGen)) -> Histogram a
sample n f =
  (Histogram . fst) $
  flip runRand (mkStdGen 33) $ do
  flip execStateT Map.empty $ do
    replicateM_ n $ do
      x <- lift $ liftRand f
      modify $ Map.insertWith (+) x 1


data Avg num = Avg {
    avg_curr :: num
  , avg_n :: Integer
  } deriving(Show, Read)

initialAvg :: (Fractional num) => Avg num
initialAvg = Avg 0 0

singletonAvg :: (Num num) => num -> Avg num
singletonAvg x = Avg x 1

current :: (Fractional s) => Avg s -> s
current (Avg c n) = c

meld :: forall s . (Fractional s) => Avg s -> s -> Avg s
meld (Avg c n) s = Avg (c + (s-c)/(fromInteger (n+1))) (n + 1)

combineAvg :: (Fractional num) => Avg num -> Avg num -> Avg num
combineAvg a@(Avg v 1) b = meld b v
combineAvg a b@(Avg v 1) = meld a v
combineAvg _ _ = error "combineAvg: Only defined for singletons for now"

-- testAvg :: Double
testAvg x = do
  -- fromRational $ do
  (current *** (\l -> sum l / (fromInteger $ toInteger $ length l))) $ do
  fst $ flip runRand (mkStdGen 0) $ do
  flip execStateT (initialAvg :: Avg Double, ([] :: [Double])) $ do
  forM_ [0..x] $ \i -> do
    -- r <- getRandomR (1,9)
    modify $ (flip meld (fromInteger i)) *** (fromInteger i:)

data Q num s a = Q {
    _q_map :: Map s (Map a (Avg num))
  } deriving(Show,Read)

makeLenses ''Q

sizeQ Q{..} = Map.size _q_map

emptyQ :: (Ord s, Ord a) => Q num s a
emptyQ = Q mempty

q2v :: (Fractional num) => Q num s a -> StateVal num s
q2v = StateVal . Map.map (sum . Map.map current) . view q_map


data Monitor num s = Monitor {
    mon_target :: StateVal num s
  , mon_data :: PlotData
  } deriving(Show)

monitorNew :: (MonadIO m) => StateVal num s -> m (Monitor num s)
monitorNew tgt = liftIO $
  Monitor tgt <$> do
    newData "MC"

-}

