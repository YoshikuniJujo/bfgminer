{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bfgminerhs.Foreign
import Bfgminerhs.Tools

import System.Environment (getProgName, getArgs)
import Control.Concurrent (threadDelay)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (when)

main :: IO ()
main = do
	r <- curlGlobalInit curlGlobalAll
	when (r /= 0) $ quit 1 "curl initialize error"

	mainInitialize =<< (:) <$> getProgName <*> getArgs

	tct <- getTotalControlThreads
	when (tct /= 7) $ quit 1 "bad total control threads"

	initialMaxStaged <- getOptQueue

	loop (initialMaxStaged, False) $ \(maxStaged, lagging) -> do
		cp <- currentPool
		(b, (ts, maxStaged', lagging')) <-
			mainInsideBool cp maxStaged lagging
		when b $ do
			work <- makeWork
			incGetfailOccasionsAndTotalGo cp lagging'
			pool <- selectPool lagging'
			doWhile $ do
				hasStratum <- doesPoolHaveStratum pool
				if hasStratum
					then poolHasStratum pool work >> return False
					else poolNotHasStratum ts maxStaged' pool work
		return (maxStaged', lagging')

	curlGlobalCleanup
