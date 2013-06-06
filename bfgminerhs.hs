{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bfgminerhs.Foreign
import Bfgminerhs.Tools

import System.Environment (getProgName, getArgs)
import Control.Concurrent (threadDelay)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (when)

poolNotHasStratum :: Int -> Int -> Pool -> Work -> IO (Pool, Bool)
poolNotHasStratum ts maxStaged pool work = do
	b <- poolNotHasStratumBody ts maxStaged pool work
	(pool', retry) <- if b then return (pool, False) else do
		ncnb <- notShouldRollBody pool work
		if not ncnb then return (pool, False) else do
			(ce, b) <- notCloneNotBenchBody ts maxStaged pool work
			p' <- if b then notGetUpstreamWork pool ce else
				return pool
			return (p', b)
	return (pool, retry)

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
			doWhile pool $ \p -> do
				hasStratum <- doesPoolHaveStratum p
				if hasStratum
					then do	poolHasStratum p work
						return (p, False)
					else do b <- doesExistLastWorkCopy p
						if b	then poolNotHasStratum ts
								maxStaged' p work
							else return (p, False)
		return (maxStaged', lagging')

	curlGlobalCleanup
