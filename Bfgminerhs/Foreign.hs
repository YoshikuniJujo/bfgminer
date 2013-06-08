module Bfgminerhs.Foreign (
	PoolProtocol(..),
	PThreadMutexT,

	applog,
	logWarning,
	logNotice,
	logInfo,
	logDebug,

	mainInitialize,

	genStratumWork,

	poolHasStratum,
	poolStratumActive,
	poolStratumNotify,
	poolLastWorkLock,
	poolLastWorkCopy,

	withMutexLock,
	mutexLock,
	mutexUnlock,

	canRoll,
	shouldRoll,
	mainDoRoll,
	mainNoRoll,

	notCloneNotBenchBody,
	cloneAvailable,
	freeWork,
	stageWork,
	getBenchmarkWork,
	getOptBenchmark,
	notGetUpstreamWork,

	poolProto,
	plpGetblocktemplate,
	getStagedRollable,
	getMiningThreads,

	_getTotalStaged,
	getOptFailOnly,
	getGwsCond,
	pThreadCondWait,

	getStgdLock,
	getCurrentPool,
	makeWork,

	poolTset,
	incPoolGetfailOccasions,
	incTotalGo,
	poolLagging,
	poolPoolNo,

	selectPool,
	curlGlobalInit,
	curlGlobalAll,
	getTotalControlThreads,
	quit,
	getOptQueue,
	curlGlobalCleanup,

	Ptr, Pool(..), Work(..),
) where

import Control.Applicative
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal (alloca, allocaArray, pokeArray)
import Foreign.Storable
-- import Data.Maybe
	
data Pool = Pool { getPtrPool :: Ptr Pool } deriving Eq
data Work = Work { getPtrWork :: Ptr Work }
data CurlEnt = CurlEnt (Ptr CurlEnt)
-- data CurlEnt = CurlEnt { getPtrCurlEnt :: Ptr CurlEnt }
data PThreadMutexT = PThreadMutexT { getPtrPThreadMutexT :: Ptr PThreadMutexT }
data PBool = PBool (Ptr Bool)
data PoolProtocol = PlpNone | PlpGetwork | PlpGetblocktemplate deriving Enum

fromCArrayFun :: (CInt -> Ptr CString -> IO a) -> [String] -> IO a
fromCArrayFun f args = do
	cArgs <- mapM newCString args
	allocaArray (length args) $ \pArgs -> do
		pokeArray pArgs cArgs
		f (fromIntegral $ length args) pArgs

mainInitialize :: [String] -> IO ()
mainInitialize = fromCArrayFun cMainInitialize

foreign import ccall "applog" cApplog :: CInt -> CString -> IO ()

applog :: Int -> String -> IO ()
applog logLevel cont = do
	cCont <- newCString cont
	cApplog (fromIntegral logLevel) cCont

logWarning, logNotice, logInfo, logDebug :: Int
logWarning = 4
logNotice = 5
logInfo = 6
logDebug = 7

foreign import ccall "curl_global_init" cCurlGlobalInit :: CInt -> IO CInt
foreign import ccall "curl_global_cleanup" cCurlGlobalCleanup :: IO ()
foreign import ccall "curl_global_all" cCurlGlobalAll :: CInt

curlGlobalInit :: Int -> IO Int
curlGlobalInit = fmap fromIntegral . cCurlGlobalInit . fromIntegral
curlGlobalCleanup :: IO ()
curlGlobalCleanup = cCurlGlobalCleanup
curlGlobalAll :: Int
curlGlobalAll = fromIntegral cCurlGlobalAll

foreign import ccall "quit" cQuit :: CInt -> CString -> IO ()
foreign import ccall "get_total_control_threads" cGetTotalControlThreads :: IO CInt

quit :: Int -> String -> IO ()
quit n msg = cQuit (fromIntegral n) =<< newCString msg
getTotalControlThreads :: IO Int
getTotalControlThreads = fromIntegral <$> cGetTotalControlThreads

foreign import ccall "main_initialize" cMainInitialize ::
	CInt -> Ptr CString -> IO ()

foreign import ccall "get_stgd_lock" cGetStgdLock :: IO (Ptr PThreadMutexT)
foreign import ccall "current_pool" cCurrentPool :: IO (Ptr Pool)

foreign import ccall "pool_proto" cPoolProto :: Ptr Pool -> IO CInt
foreign import ccall "plp_getblocktemplate" cPlpGetblocktemplate :: CInt
foreign import ccall "get_staged_rollable" cGetStagedRollable :: IO CInt
foreign import ccall "get_mining_threads" cGetMiningThreads :: IO CInt

poolProto :: Pool -> IO PoolProtocol
poolProto = fmap (toEnum . fromIntegral) . cPoolProto . getPtrPool
plpGetblocktemplate :: PoolProtocol
plpGetblocktemplate = toEnum $ fromIntegral cPlpGetblocktemplate
getStagedRollable, getMiningThreads :: IO Int
getStagedRollable = fromIntegral <$> cGetStagedRollable
getMiningThreads = fromIntegral <$> cGetMiningThreads

foreign import ccall "wrap___total_staged" c_TotalStaged :: IO CInt
foreign import ccall "get_opt_fail_only" cGetOptFailOnly :: IO Bool
foreign import ccall "get_gws_cond" cGetGwsCond :: IO (Ptr PThreadMutexT)
foreign import ccall "pthread_cond_wait" cPThreadCondWait ::
	Ptr PThreadMutexT -> Ptr PThreadMutexT -> IO ()

_getTotalStaged :: IO Int
_getTotalStaged = fromIntegral <$> c_TotalStaged
getOptFailOnly :: IO Bool
getOptFailOnly = cGetOptFailOnly
getGwsCond :: IO PThreadMutexT
getGwsCond = PThreadMutexT <$> cGetGwsCond
pThreadCondWait :: PThreadMutexT -> PThreadMutexT -> IO ()
pThreadCondWait (PThreadMutexT cond) (PThreadMutexT lock) =
	cPThreadCondWait cond lock

getStgdLock :: IO PThreadMutexT
getStgdLock = PThreadMutexT <$> cGetStgdLock
getCurrentPool :: IO Pool
getCurrentPool = Pool <$> cCurrentPool

foreign import ccall "wrap_make_work" cMakeWork :: IO (Ptr Work)
foreign import ccall "wrap_select_pool" cSelectPool :: Bool -> IO (Ptr Pool)
foreign import ccall "wrap_pool_tset" cPoolTset ::
	Ptr Pool -> Ptr Bool -> IO Bool
foreign import ccall "inc_pool_getfail_occasions" cIncPoolGetfailOccasions ::
	Ptr Pool -> IO ()
foreign import ccall "inc_total_go" cIncTotalGo :: IO ()
foreign import ccall "pool_lagging" cPoolLagging :: Ptr Pool -> IO (Ptr Bool)
foreign import ccall "pool_pool_no" cPoolPoolNo :: Ptr Pool -> IO CInt

makeWork :: IO Work
makeWork = Work <$> cMakeWork
selectPool :: Bool -> IO Pool
selectPool = fmap Pool . cSelectPool
poolTset :: Pool -> PBool -> IO Bool
poolTset (Pool pp) (PBool pb) = cPoolTset pp pb
incPoolGetfailOccasions :: Pool -> IO ()
incPoolGetfailOccasions = cIncPoolGetfailOccasions . getPtrPool
incTotalGo :: IO ()
incTotalGo = cIncTotalGo
poolLagging :: Pool -> IO PBool
poolLagging = fmap PBool . cPoolLagging . getPtrPool
poolPoolNo :: Pool -> IO Int
poolPoolNo = fmap fromIntegral . cPoolPoolNo . getPtrPool

foreign import ccall "wrap_gen_stratum_work" cGenStratumWork ::
	Ptr Pool -> Ptr Work -> IO ()

foreign import ccall "pool_has_stratum" cPoolHasStratum :: Ptr Pool -> IO Bool
foreign import ccall "pool_stratum_active" cPoolStratumActive :: Ptr Pool -> IO Bool
foreign import ccall "pool_stratum_notify" cPoolStratumNotify :: Ptr Pool -> IO Bool
foreign import ccall "pool_last_work_lock" cPoolLastWorkLock ::
	Ptr Pool -> IO (Ptr PThreadMutexT)
foreign import ccall "pool_last_work_copy" cPoolLastWorkCopy ::
	Ptr Pool -> IO (Ptr Work)

foreign import ccall "wrap_mutex_lock" cMutexLock :: Ptr PThreadMutexT -> IO ()
foreign import ccall "wrap_mutex_unlock" cMutexUnlock :: Ptr PThreadMutexT -> IO ()

foreign import ccall "wrap_can_roll" cCanRoll :: Ptr Work -> IO Bool
foreign import ccall "wrap_should_roll" cShouldRoll :: Ptr Work -> IO Bool

canRoll, shouldRoll :: Work -> IO Bool
canRoll = cCanRoll . getPtrWork
shouldRoll = cShouldRoll . getPtrWork

foreign import ccall "main_do_roll" cMainDoRoll :: Ptr Pool -> Ptr Work -> IO ()
foreign import ccall "main_not_roll" cMainNoRoll :: Ptr Pool -> Ptr Work -> IO ()

mainDoRoll, mainNoRoll :: Pool -> Work -> IO ()
mainDoRoll (Pool pp) (Work pw) = cMainDoRoll pp pw
mainNoRoll (Pool pp) (Work pw) = cMainNoRoll pp pw

mutexLock, mutexUnlock :: PThreadMutexT -> IO ()
mutexLock = cMutexLock . getPtrPThreadMutexT
mutexUnlock = cMutexUnlock . getPtrPThreadMutexT
		
withMutexLock :: IO PThreadMutexT -> IO a -> IO a
withMutexLock getLock action = do
	lock <- getLock
	mutexLock lock
	r <- action
	mutexUnlock lock
	return r

genStratumWork :: Pool -> Work -> IO ()
genStratumWork (Pool pp) (Work pw) = cGenStratumWork pp pw

poolHasStratum, poolStratumActive, poolStratumNotify :: Pool -> IO Bool
poolHasStratum = cPoolHasStratum . getPtrPool
poolStratumActive = cPoolStratumActive . getPtrPool
poolStratumNotify = cPoolStratumNotify . getPtrPool
poolLastWorkLock :: Pool -> IO PThreadMutexT
poolLastWorkLock = fmap PThreadMutexT . cPoolLastWorkLock . getPtrPool
poolLastWorkCopy :: Pool -> IO (Maybe Work)
poolLastWorkCopy (Pool pp) = do
	pw <- cPoolLastWorkCopy pp
	return $ if pw == nullPtr then Nothing else Just $ Work pw

foreign import ccall "not_clone_not_bench_body" cNotCloneNotBenchBody ::
	CInt -> CInt -> Ptr Pool -> Ptr Work -> Ptr (Ptr CurlEnt) -> IO Bool
foreign import ccall "wrap_clone_available" cCloneAvailable :: IO Bool
foreign import ccall "free_work" cFreeWork :: Ptr Work -> IO ()
foreign import ccall "wrap_get_benchmark_work" cGetBenchmarkWork :: Ptr Work -> IO ()
foreign import ccall "wrap_stage_work" cStageWork :: Ptr Work -> IO ()
foreign import ccall "get_opt_benchmark" cGetOptBenchmark :: IO Bool
foreign import ccall "not_get_upstream_work" cNotGetUpstreamWork ::
	Ptr (Ptr Pool) -> Ptr CurlEnt -> IO ()

notCloneNotBenchBody :: Int -> Int -> Pool -> Work -> IO (CurlEnt, Bool)
notCloneNotBenchBody ts maxStaged (Pool p) (Work w) = alloca $ \pce -> do
	b <- cNotCloneNotBenchBody (fromIntegral ts) (fromIntegral maxStaged) p w pce
	ce <- peek pce
	return (CurlEnt ce, b)
cloneAvailable, getOptBenchmark :: IO Bool
cloneAvailable = cCloneAvailable
freeWork, getBenchmarkWork, stageWork :: Work -> IO ()
freeWork = cFreeWork . getPtrWork
getBenchmarkWork = cGetBenchmarkWork . getPtrWork
stageWork = cStageWork . getPtrWork

getOptBenchmark = cGetOptBenchmark

notGetUpstreamWork :: Pool -> CurlEnt -> IO Pool
notGetUpstreamWork (Pool p) (CurlEnt ce) = alloca $ \pp -> do
	poke pp p
	cNotGetUpstreamWork pp ce
	p' <- peek pp
	return $ Pool p'

foreign import ccall "get_opt_queue" cGetOptQueue :: IO CInt
getOptQueue :: IO Int
getOptQueue = fromIntegral <$> cGetOptQueue
