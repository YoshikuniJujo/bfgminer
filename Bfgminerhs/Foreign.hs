module Bfgminerhs.Foreign (
	applog,
	logNotice,
	logInfo,
	logDebug,

	mainInitialize,
	doesPoolHaveStratum,

	genStratumWork,
	poolHasStratum,
	poolStratumActive,
	poolStratumNotify,

	doesExistLastWorkCopy,
	poolNotHasStratumBody,
	notCloneNotBenchBody,
	cloneAvailable,
	freeWork,
	stageWork,
	getBenchmarkWork,
	getOptBenchmark,
	notGetUpstreamWork,
	mainInsideBool,
	currentPool,
	makeWork,
	incGetfailOccasionsAndTotalGo,
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
	
data Pool = Pool { getPtrPool :: Ptr Pool } deriving Eq
data Work = Work { getPtrWork :: Ptr Work }
data CurlEnt = CurlEnt (Ptr CurlEnt)
-- data CurlEnt = CurlEnt { getPtrCurlEnt :: Ptr CurlEnt }

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

logNotice, logInfo, logDebug :: Int
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

foreign import ccall "main_inside_bool" cMainInsideBool ::
	Ptr CInt -> Ptr Pool -> Ptr CInt -> Ptr Bool -> IO ()
foreign import ccall "current_pool" cCurrentPool :: IO (Ptr Pool)

mainInsideBool :: Pool -> Int -> Bool -> IO (Int, Int, Bool)
mainInsideBool (Pool p) maxStaged lagging =
	alloca $ \pts -> alloca $ \pms -> alloca $ \pl -> do
		poke pms $ fromIntegral maxStaged
		poke pl lagging
		cMainInsideBool pts p pms pl
		ts <- fromIntegral <$> peek pts
		maxStaged' <- fromIntegral <$> peek pms
		lagging' <- peek pl
		return (ts, maxStaged', lagging')
currentPool :: IO Pool
currentPool = Pool <$> cCurrentPool

foreign import ccall "wrap_make_work" cMakeWork :: IO (Ptr Work)
foreign import ccall "wrap_select_pool" cSelectPool :: Bool -> IO (Ptr Pool)
foreign import ccall "inc_getfail_occasions_and_total_go"
	cIncGetfailOccasionsAndTotalGo :: Ptr Pool -> Bool -> IO ()

makeWork :: IO Work
makeWork = Work <$> cMakeWork
selectPool :: Bool -> IO Pool
selectPool = fmap Pool . cSelectPool
incGetfailOccasionsAndTotalGo :: Pool -> Bool -> IO ()
incGetfailOccasionsAndTotalGo = cIncGetfailOccasionsAndTotalGo . getPtrPool

foreign import ccall "does_pool_have_stratum" cDoesPoolHaveStratum ::
	Ptr Pool -> IO Bool

foreign import ccall "wrap_gen_stratum_work" cGenStratumWork ::
	Ptr Pool -> Ptr Work -> IO ()
foreign import ccall "pool_has_stratum" cPoolHasStratum :: Ptr Pool -> IO Bool
foreign import ccall "pool_stratum_active" cPoolStratumActive :: Ptr Pool -> IO Bool
foreign import ccall "pool_stratum_notify" cPoolStratumNotify :: Ptr Pool -> IO Bool

genStratumWork :: Pool -> Work -> IO ()
genStratumWork (Pool pp) (Work pw) = cGenStratumWork pp pw
poolHasStratum, poolStratumActive, poolStratumNotify :: Pool -> IO Bool
poolHasStratum = cPoolHasStratum . getPtrPool
poolStratumActive = cPoolStratumActive . getPtrPool
poolStratumNotify = cPoolStratumNotify . getPtrPool

foreign import ccall "does_exist_last_work_copy" cDoesExistLastWorkCopy ::
	Ptr Pool -> IO Bool
foreign import ccall "pool_not_has_stratum_body" cPoolNotHasStratumBody ::
	CInt -> CInt -> Ptr Pool -> Ptr Work -> IO Bool
foreign import ccall "not_clone_not_bench_body" cNotCloneNotBenchBody ::
	CInt -> CInt -> Ptr Pool -> Ptr Work -> Ptr (Ptr CurlEnt) -> IO Bool
foreign import ccall "wrap_clone_available" cCloneAvailable :: IO Bool
foreign import ccall "free_work" cFreeWork :: Ptr Work -> IO ()
foreign import ccall "wrap_get_benchmark_work" cGetBenchmarkWork :: Ptr Work -> IO ()
foreign import ccall "wrap_stage_work" cStageWork :: Ptr Work -> IO ()
foreign import ccall "get_opt_benchmark" cGetOptBenchmark :: IO Bool
foreign import ccall "not_get_upstream_work" cNotGetUpstreamWork ::
	Ptr (Ptr Pool) -> Ptr CurlEnt -> IO ()

doesPoolHaveStratum :: Pool -> IO Bool
doesPoolHaveStratum = cDoesPoolHaveStratum . getPtrPool
doesExistLastWorkCopy :: Pool -> IO Bool
doesExistLastWorkCopy = cDoesExistLastWorkCopy . getPtrPool
poolNotHasStratumBody :: Int -> Int -> Pool -> Work -> IO Bool
poolNotHasStratumBody ts maxStaged (Pool p) (Work w) =
	cPoolNotHasStratumBody (fromIntegral ts) (fromIntegral maxStaged) p w

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
