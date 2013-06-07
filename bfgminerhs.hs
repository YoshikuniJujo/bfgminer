{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bfgminerhs.Foreign
import Bfgminerhs.Tools

import System.Environment (getProgName, getArgs)
import Control.Concurrent (threadDelay)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (when)

notShouldRollBody :: Int -> Int -> Pool -> Work -> IO (Pool, Bool)
notShouldRollBody ts maxStaged pool work = do
	ca <- cloneAvailable
	if ca then do
		applog logDebug "Cloned getwork work"
		freeWork work
		return (pool, False)
	else do
		ob <- getOptBenchmark
		if ob then do
			getBenchmarkWork work
			applog logDebug "Generated benchmark work"
			stageWork work
			return (pool, False)
		else do
			(ce, b') <- notCloneNotBenchBody ts maxStaged pool work
			p' <- if b' then notGetUpstreamWork pool ce else
				return pool
			return (p', b')

poolNotHasStratum :: Int -> Int -> Pool -> Work -> IO (Pool, Bool)
poolNotHasStratum ts maxStaged pool work = do
	lock <- poolLastWorkLock pool
	mutexLock lock
	lastWork <- poolLastWorkCopy pool
	b <- poolNotHasStratumBody' pool work lastWork
	mutexUnlock lock
	if b	then return (pool, False)
		else notShouldRollBody ts maxStaged pool work

poolNotHasStratumBody' :: Pool -> Work -> Maybe Work -> IO Bool
poolNotHasStratumBody' pool work (Just lastWork) = do
	cr <- canRoll lastWork
	if cr then do
		sr <- shouldRoll lastWork
		if sr then mainDoRoll pool work >> return True
		else mainNoRoll pool lastWork >> return False
	else mainNoRoll pool lastWork >> return False
poolNotHasStratumBody' _ _ Nothing = return False

funPoolHasStratum' :: Pool -> Work -> IO ()
funPoolHasStratum' pool work = do
	pool' <- while pool check $ \p -> do
		altpool <- selectPool True
		when (altpool == p) $ threadDelay 5000000
		return altpool
	genStratumWork pool' work
	applog logDebug "Generated stratum work"
	stageWork work
	where
	check p = do
		b0 <- poolHasStratum p
		if b0 then do
			b1 <- poolStratumActive p
			if b1 then do
				b2 <- poolStratumNotify p
				if b2 then return False else return True
			else return True
		else return False

undef :: a
undef = undefined

incGetfailOccasionsAndTotalGo :: Pool -> Bool -> IO ()
incGetfailOccasionsAndTotalGo cp True = do
	cpLagging <- poolLagging cp
	b <- poolTset cp cpLagging
	when b $ do
		pn <- poolPoolNo cp
		applog logWarning $ "Pool " ++ show pn ++
			" not providing work fast enough"
		incPoolGetfailOccasions cp
		incTotalGo
incGetfailOccasionsAndTotalGo _ False = return ()

enlargeMaxStaged :: Pool -> Int -> IO Int
enlargeMaxStaged cp maxStaged = do
	-- If the primary pool is a getwork pool and cannot roll work,
	-- try to stage one extra work per mining thread
	phs <- poolHasStratum cp
	if phs then return maxStaged else do
		pp <- poolProto cp
		case pp of
			PlpGetblocktemplate -> return maxStaged
			_ -> do	sr <- getStagedRollable
				if sr /= 0 then return maxStaged
				else (maxStaged +) <$> getMiningThreads

setLaggingEtc :: Pool -> Int -> Bool -> IO (Int, Bool)
setLaggingEtc cp maxStaged lagging = do
	ts <- _totalStaged
	phs <- poolHasStratum cp
	lagging' <- if phs then return lagging else do
		pp <- poolProto cp
		case pp of
			PlpGetblocktemplate -> return lagging
			_ -> if ts /= 0 then return lagging else do
				fo <- getOptFailOnly
				if fo then return lagging else return True
	-- Wait until hash_pop tells us we need to create more work
	ts' <- if ts > maxStaged then do
			gc <- getGwsCond
			sl <- getStgdLock
			pThreadCondWait gc sl
			_totalStaged
		else return ts
	return (ts', lagging')
		

main :: IO ()
main = do
	r <- curlGlobalInit curlGlobalAll
	when (r /= 0) $ quit 1 "curl initialize error"

	mainInitialize =<< (:) <$> getProgName <*> getArgs

	tct <- getTotalControlThreads
	when (tct /= 7) $ quit 1 "bad total control threads"

	initialMaxStaged <- getOptQueue

	_ <- loop (initialMaxStaged, False) $ \(maxStaged, lagging) -> do
		(cp, ts, maxStaged', lagging') <- doWhile
			(undef, undef, maxStaged, lagging) $ \(_, _, ms, l) -> do
				cp_ <- currentPool
				stgdLock <- getStgdLock
				mutexLock stgdLock
				ms' <- enlargeMaxStaged cp_ ms
				(ts_, l') <- setLaggingEtc cp_ ms' l
				mutexUnlock stgdLock
				return ((cp_, ts_, ms', l'), ts_ > ms')
		work <- makeWork
		incGetfailOccasionsAndTotalGo cp lagging'
		pool <- selectPool lagging'
		_ <- doWhile pool $ \p -> do
			hasStratum <- poolHasStratum p
			if hasStratum
				then do	funPoolHasStratum' p work
					return (p, False)
				else do b <- poolLastWorkCopy p
					case b of
						Just _ -> poolNotHasStratum ts
							maxStaged' p work
						_ -> return (p, False)
		return (maxStaged', lagging')

	curlGlobalCleanup
