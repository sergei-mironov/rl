{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Rnd where

import Control.Monad.Identity
import Control.Monad.State.Strict
import Control.Break
import System.Random
-- import Imports

class (Monad m, RandomGen g) => MonadRnd g m | m -> g where
  roll :: (g -> (a,g)) -> m a
  getGen :: m g
  putGen :: g -> m ()

getRndR :: (MonadRnd g m, Random a) => (a,a) -> m a
getRndR = roll . randomR

newtype RndT g m a = RndT { unRndT :: StateT g m a }
    deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadFix)

runRnd :: RndT g Identity a -> g -> (a,g)
runRnd r g = runIdentity $ runStateT (unRndT r) g

runRndT :: RndT g m a -> g -> m (a,g)
runRndT r g = runStateT (unRndT r) g

evalRndT :: (Monad m) => RndT g m a -> g -> m a
evalRndT r g = fst <$> runRndT r g

evalRndT_ r g = evalRndT r g  >> return ()

instance (Monad m, RandomGen g) => MonadRnd g (RndT g m) where
  getGen = RndT get
  putGen = RndT .  put
  roll f = RndT $ do
    g <- get
    (a,g') <- pure (f g)
    put g'
    return a

rollM :: (MonadRnd g m) => (g -> m (a, g)) -> m a
rollM mf = do
  g <- getGen
  (a,g') <- mf g
  putGen g'
  return a

instance (MonadRnd g m) => MonadRnd g (StateT s m) where
  getGen = lift getGen
  putGen = lift . putGen
  roll = lift . roll

instance (MonadRnd g m) => MonadRnd g (Break r m) where
  getGen = lift getGen
  putGen = lift . putGen
  roll = lift . roll

-- | Extracted from MonadRandom AS-IS
-- Sample a random value from a weighted list.  The total weight of all
-- elements must not be 0.
fromList :: (MonadRnd g m) => [(a,Rational)] -> m a
fromList [] = error "MonadRnd.fromList called with empty list"
fromList [(x,_)] = return x
fromList xs = do
  -- TODO: Do we want to be able to use floats as weights?
  -- TODO: Better error message if weights sum to 0.
  let s = (fromRational (sum (map snd xs))) :: Double -- total weight
      cs = scanl1 (\(_,q) (y,s') -> (y, s'+q)) xs       -- cumulative weight
  p <- liftM toRational $ getRndR (0.0,s)
  return . fst . head $ dropWhile (\(_,q) -> q < p) cs

-- | Sample a value from a uniform distribution of a list of elements.
uniform :: (MonadRnd g m) => [a] -> m a
uniform = Control.Monad.Rnd.fromList . fmap (flip (,) 1)

