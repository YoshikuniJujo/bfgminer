module Bfgminerhs.Foreign (
	mainInitialize,
	doesPoolHaveStratum,
	poolHasStratum,
	poolNotHasStratum,
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
	
data Pool = Pool { getPtrPool :: Ptr Pool }
data Work = Work { getPtrWork :: Ptr Work }

fromCArrayFun :: (CInt -> Ptr CString -> IO a) -> [String] -> IO a
fromCArrayFun f args = do
	cArgs <- mapM newCString args
	allocaArray (length args) $ \pArgs -> do
		pokeArray pArgs cArgs
		f (fromIntegral $ length args) pArgs

mainInitialize :: [String] -> IO ()
mainInitialize = fromCArrayFun cMainInitialize

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
	Ptr CInt -> Ptr Pool -> Ptr CInt -> Ptr Bool -> IO Bool
foreign import ccall "current_pool" cCurrentPool :: IO (Ptr Pool)

mainInsideBool :: Pool -> Int -> Bool -> IO (Bool, (Int, Int, Bool))
mainInsideBool (Pool p) maxStaged lagging =
	alloca $ \pts -> alloca $ \pms -> alloca $ \pl -> do
		poke pms $ fromIntegral maxStaged
		poke pl lagging
		b <- cMainInsideBool pts p pms pl
		ts <- fromIntegral <$> peek pts
		maxStaged' <- fromIntegral <$> peek pms
		lagging' <- peek pl
		return (b, (ts, maxStaged', lagging'))
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
foreign import ccall "pool_has_stratum" cPoolHasStratum ::
	Ptr Pool -> Ptr Work -> IO ()
foreign import ccall "pool_not_has_stratum" cPoolNotHasStratum ::
	CInt -> CInt -> Ptr Pool -> Ptr Work -> IO Bool

doesPoolHaveStratum :: Pool -> IO Bool
doesPoolHaveStratum = cDoesPoolHaveStratum . getPtrPool
poolHasStratum :: Pool -> Work -> IO ()
poolHasStratum (Pool p) (Work w) = cPoolHasStratum p w
poolNotHasStratum :: Int -> Int -> Pool -> Work -> IO Bool
poolNotHasStratum ts maxStaged (Pool p) (Work w) =
	cPoolNotHasStratum (fromIntegral ts) (fromIntegral maxStaged) p w

foreign import ccall "get_opt_queue" cGetOptQueue :: IO CInt

getOptQueue :: IO Int
getOptQueue = fromIntegral <$> cGetOptQueue
