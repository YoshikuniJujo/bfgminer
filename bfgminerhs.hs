{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import Control.Concurrent
import Control.Applicative
import Control.Monad
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal hiding (Pool)
import Foreign.Storable

foreign import ccall "curl_global_init" cCurlGlobalInit :: CInt -> IO CInt
foreign import ccall "curl_global_cleanup" cCurlGlobalCleanup :: IO ()
foreign import ccall "curl_global_all" cCurlGlobalAll :: CInt

foreign import ccall "quit" cQuit :: CInt -> CString -> IO ()
foreign import ccall "get_total_control_threads" cGetTotalControlThreads :: IO CInt

foreign import ccall "main_initialize" cMainInitialize ::
	CInt -> Ptr CString -> IO ()

mainInitialize :: [String] -> IO ()
mainInitialize = fromCArrayFun cMainInitialize

foreign import ccall "main_inside_bool" cMainInsideBool ::
	Ptr CInt -> Ptr Pool -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall "current_pool" cCurrentPool :: IO (Ptr Pool)

foreign import ccall "fun_make_work" cMakeWork :: IO (Ptr Work)
foreign import ccall "fun_select_pool" cSelectPool :: CInt -> IO (Ptr Pool)
foreign import ccall "inc_getfail_occasions_and_total_go"
	cIncGetfailOccasionsAndTotalGo :: Ptr Pool -> CInt -> IO ()

foreign import ccall "pool_has_stratum" cPoolHasStratum ::
	Ptr Pool -> Ptr Work -> IO ()
foreign import ccall "pool_not_has_stratum" cPoolNotHasStratum ::
	CInt -> CInt -> Ptr Pool -> Ptr Work -> IO Bool
foreign import ccall "does_pool_have_stratum" cDoesPoolHaveStratum ::
	Ptr Pool -> IO Bool

mainInsideBodyBody :: CInt -> CInt -> Ptr Pool -> Ptr Work -> IO ()
mainInsideBodyBody ts maxStaged pool work = do
	hasStratum <- cDoesPoolHaveStratum pool
	if hasStratum then cPoolHasStratum pool work else do
		retry <- cPoolNotHasStratum ts maxStaged pool work
		when retry $ mainInsideBodyBody ts maxStaged pool work
	
data Pool
data Work

foreign import ccall "get_opt_queue" cGetOptQueue :: IO CInt

mainInsideBool :: Ptr CInt -> Ptr Pool -> Ptr CInt -> Ptr CInt -> IO Bool
mainInsideBool ts cp maxStaged lagging =
	(/= 0) <$>  cMainInsideBool ts cp maxStaged lagging

mainInside :: CInt -> CInt -> IO (CInt, CInt)
mainInside v1 v2 = alloca $ \ts -> alloca $ \p1 -> alloca $ \p2 -> do
	poke p1 v1
	poke p2 v2
	cp <- cCurrentPool
	b <- mainInsideBool ts cp p1 p2
	tsv <- peek ts
	v1' <- peek p1
	v2' <- peek p2
	when b $ mainInsideBody tsv cp v1' v2'
	return (v1', v2')

mainInsideBody :: CInt -> Ptr Pool -> CInt -> CInt -> IO ()
mainInsideBody ts cp maxStaged lagging = do
	work <- cMakeWork
	cIncGetfailOccasionsAndTotalGo cp lagging
	pool <- cSelectPool lagging
	mainInsideBodyBody ts maxStaged pool work

main :: IO ()
main = do
	progName <- getProgName
	args <- getArgs
	putStrLn "begin haskell! delete no arg"
	threadDelay 1000000

	cCurlGlobalInit cCurlGlobalAll >>= print
	threadDelay 1000000

	mainInitialize $ progName : args

	tct <- cGetTotalControlThreads
	when (tct /= 7) . cQuit 1 =<< newCString "bad total control threads"

	initMaxStaged <- cGetOptQueue

	loop (initMaxStaged, 0) (uncurry mainInside)

	cCurlGlobalCleanup

loop :: a -> (a -> IO a) -> IO a
loop x act = do
	r <- act x
	loop r act

fromCArrayFun :: (CInt -> Ptr CString -> IO a) -> [String] -> IO a
fromCArrayFun f args = do
	cArgs <- mapM newCString args
	allocaArray (length args) $ \pArgs -> do
		pokeArray pArgs cArgs
		f (fromIntegral $ length args) pArgs
