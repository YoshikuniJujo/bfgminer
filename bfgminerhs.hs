{-# LANGUAGE OverloadedStrings, TupleSections #-}

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
			workSetPool work pool
			ce <- popCurlEntry3 pool 2
			curl <- curlEntCurl ce
			-- obtain new work from bitcoin via JSON-RPC
			ifM (getUpstreamWork work curl) (do
				pl <- poolLagging pool
				when (ts >= maxStaged) $
					poolTClear pool pl >> return ()
				pIdle <- poolIdle pool
				whenM (poolTClear pool pIdle) $ poolResus pool
				applog logDebug "Generated getwork work"
				stageWork work
				pushCurlEntry ce pool
				return (pool, False)) (do
				pool' <- notGetUpstreamWork pool ce
				return (pool', True))

poolNotHasStratum :: Int -> Int -> Pool -> Work -> IO (Pool, Bool)
poolNotHasStratum ts maxStaged pool work = do
	lock <- poolLastWorkLock pool
	mutexLock lock
	lastWork <- poolLastWorkCopy pool
	b <- poolNotHasStratumBody pool work lastWork
	mutexUnlock lock
	if b	then return (pool, False)
		else notShouldRollBody ts maxStaged pool work

poolNotHasStratumBody :: Pool -> Work -> Maybe Work -> IO Bool
poolNotHasStratumBody pool work (Just lastWork) = do
	cr <- canRoll lastWork
	if cr then do
		sr <- shouldRoll lastWork
		if sr then mainDoRoll pool work >> return True
		else mainNoRoll pool lastWork >> return False
	else mainNoRoll pool lastWork >> return False
poolNotHasStratumBody _ _ Nothing = return False

mainDoRoll :: Pool -> Work -> IO ()
mainDoRoll pool work = do
	freeWork work
	work' <- makeClone =<< poolLastWorkCopy pool
	mutexUnlock =<< poolLastWorkLock pool
	_ <- rollWork work'
	wt <- workTmpl work'
	t <- time
	tl <- blkmkTimeLeft wt t
	applog logDebug $ "Generated work from latest GBT job in " ++
		"get_work_thread with " ++ show tl ++ " seconds left"
	stageWork work'

mainNoRoll :: Pool -> Work -> IO ()
mainNoRoll pool lastWork = do
	lwt <- workTmpl lastWork
	pp <- poolProto pool
	bwl <- blkmkWorkLeft lwt
	mts <- getMiningThreads
	case (lwt, pp, bwl > mts) of
		(Just _, PlpGetblocktemplate, True) -> return ()
		_ -> do	freeWork lastWork
			poolClearLastWorkCopy pool

funPoolHasStratum :: Pool -> Work -> IO ()
funPoolHasStratum pool work = do
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
			if b1 then not <$> poolStratumNotify p else return True
		else return False

undef :: a
undef = undefined

incGetfailOccasionsAndTotalGo :: Pool -> Bool -> IO ()
incGetfailOccasionsAndTotalGo cp True = do
	cpLagging <- poolLagging cp
	b <- poolTSet cp cpLagging
	when b $ do
		pn <- poolPoolNo cp
		applog logWarning $ "Pool " ++ show pn ++
			" not providing work fast enough"
		incPoolGetfailOccasions cp
		incTotalGo
incGetfailOccasionsAndTotalGo _ False = return ()

getworkCantRoll :: Pool -> IO Bool
getworkCantRoll currentPool = do
	-- If the primary pool is a getwork pool and cannot roll work,
	-- try to stage one extra work per mining thread
	cphs <- poolHasStratum currentPool
	cpp <- poolProto currentPool
	sr <- getStagedRollable
	return $ case (cphs, cpp, sr) of
		(True, _, _) -> False
		(_, PlpGetblocktemplate, _) -> False
		(_, _, 0) -> True
		_ -> False

setLaggingEtc :: Pool -> Int -> Bool -> IO (Int, Bool)
setLaggingEtc currentPool maxStaged lagging = do
	ts <- _getTotalStaged
	phs <- poolHasStratum currentPool
	lagging' <- if phs then return lagging else do
		pp <- poolProto currentPool
		case pp of
			PlpGetblocktemplate -> return lagging
			_ -> if ts /= 0 then return lagging else do
				fo <- getOptFailOnly
				return $ not fo || lagging
	-- Wait until hash_pop tells us we need to create more work
	ts' <- if ts > maxStaged then do
			gc <- getGwsCond
			sl <- getStgdLock
			pThreadCondWait gc sl
			_getTotalStaged
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
		(currentPool, totalStaged, maxStaged', lagging') <- doWhile
			(undef, undef, maxStaged, lagging) $ \(_, _, ms, l) -> do
				cp <- getCurrentPool
				(ts, ms', l') <- withMutexLock getStgdLock $ do
					ms_ <- ifM (getworkCantRoll cp)
						((ms +) <$> getMiningThreads)
						(return ms)
					(ts_, l_) <- setLaggingEtc cp ms_ l
					return (ts_, ms_, l_)
				return ((cp, ts, ms', l'), ts > ms')
		work <- makeWork
		incGetfailOccasionsAndTotalGo currentPool lagging'
		pool <- selectPool lagging'
		_ <- doWhile pool $ \p -> do
			hasStratum <- poolHasStratum p
			if hasStratum
				then do	funPoolHasStratum p work
					return (p, False)
				else do b <- poolLastWorkCopy p
					case b of
						Just _ -> poolNotHasStratum totalStaged
							maxStaged' p work
						_ -> return (p, False)
		return (maxStaged', lagging')

	curlGlobalCleanup
