{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
module Graphics.TinyPlot where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Control.Exception
import Data.Char
import System.IO
import System.Process
import System.FilePath
import Text.Heredoc
import Text.Printf

data PlotData = PlotData {
    ps_filename :: String
  , ps_handle :: Handle
  } deriving(Show)

newData :: FilePath -> IO PlotData
newData ((-<.> ".dat") -> filename) = PlotData filename <$> openFile filename WriteMode

push :: (MonadIO m, Fractional num, Real num) => PlotData -> num -> num -> m ()
push PlotData{..} (fromRational . toRational -> x :: Double) (fromRational . toRational -> y :: Double) = liftIO $ do
  hPutStrLn ps_handle (show x ++ "\t" ++ show y) >> hFlush ps_handle

dat :: PlotData -> String
dat PlotData{..} = printf "\"%s\"" ps_filename


data Plot = Plot {
  pl_handle :: ProcessHandle
}

spawnPlot :: String -> String -> IO Plot
spawnPlot ((-<.> ".gnuplot") -> name) plot =
  Plot <$> do
    writeFile name plot *> spawnProcess "gnuplot" [name]

withPlot :: String -> String -> IO a -> IO a
withPlot ((-<.> ".gnuplot") -> name) plot h = do
  writeFile name plot
  p <- spawnProcess "gnuplot" [name]
  r <- h `finally` terminateProcess p
  return r


test = do
  d <- newData "plot.dat"

  spawnPlot "plot1" [heredoc|
    set xrange [0:20]
    set yrange [0:400]
    done = 0
    bind all 'd' 'done = 1'
    while(!done) {
      plot ${dat d} using 1:2 with lines
      pause 1
    }
  |]

  forM_ [0..100] $ \i@(fromInteger -> r) -> do
    when (i`mod`10 == 0) $ do
      threadDelay (10^6)
    push d r (r*r  / 3.2)

