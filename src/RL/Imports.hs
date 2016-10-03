{-# LANGUAGE UndecidableInstances #-}

module RL.Imports (
    module Control.Arrow
  , module Control.Applicative
  , module Control.Concurrent
  , module Control.Concurrent.STM
  , module Control.Monad
  , module Control.Monad.Trans
  , module Control.Monad.State.Strict
  , module Control.Monad.Rnd
  , module Control.Lens
  , module Control.Monad.Loops
  , module Control.Break_RL
  , module Data.Bits
  , module Data.Ratio
  , module Data.Tuple
  , module Data.List
  , module Data.Map.Strict
  , module Data.HashMap.Strict
  , module Data.HashSet
  , module Data.Maybe
  , module Data.Set
  , module Data.Function
  , module Data.Foldable
  , module Data.Text
  , module Data.Monoid
  , module Data.Hashable
  , module Debug.Trace
  , module Prelude
  , module System.Random
  , module System.Random.Mersenne.Pure64
  , module System.Directory
  , module Text.Printf
  , module Text.Heredoc_RL
  , module Text.Show.Pretty
  , module Graphics.TinyPlot
  , module RL.Imports
  , module GHC.Generics
)

where

import Control.Arrow ((&&&),(***))
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Strict
import Control.Monad.Rnd
import Control.Lens (Lens', makeLenses, (%=), (^.), view, use, uses, zoom, _1, _2, _3, _4, _5, _6)
import Control.Monad.Loops
import Control.Break_RL
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Bits
import Data.Ratio
import Data.Tuple
import Data.List hiding (break)
import qualified Data.List as List
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Set (Set,member)
import qualified Data.Set as Set
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import Data.Maybe
import Data.Foldable
import Data.Function
import Debug.Trace hiding(traceM)
import Prelude hiding(break)
import System.Random
import System.Random.Mersenne.Pure64
import System.Directory
import Text.Printf
import Text.Heredoc_RL
import Text.Show.Pretty
import Graphics.TinyPlot
import Data.Text (Text)
import Data.Monoid ((<>))
import Data.Hashable
import GHC.Generics (Generic)


trace1 :: (Show a) => a -> a
trace1 a = trace (ppShow a) a

traceM :: (Monad m, Show a) => a -> m ()
traceM a = trace (ppShow a) (return ())

trace' :: (Show a) => a -> b -> b
trace' a b = trace (ppShow a) b

loopM s0 f m = iterateUntilM (not . f) m s0


iter  :: Monad m => r -> (r -> Break r m r) -> m r
iter r0 f = do
    x <- runExceptT (unBreak (f r0))
    case x of
        Left  r -> return r
        Right r -> iter r f

instance (MonadRnd g m) => MonadRnd g (Break r m) where
  getGen = lift getGen
  putGen = lift . putGen
  roll = lift . roll

