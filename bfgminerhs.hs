{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import Control.Concurrent
import Control.Applicative
import Control.Monad
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable

foreign import ccall "curl_global_init" cCurlGlobalInit :: CInt -> IO CInt
foreign import ccall "curl_global_cleanup" cCurlGlobalCleanup :: IO ()
foreign import ccall "curl_global_all" cCurlGlobalAll :: CInt

foreign import ccall "quit" cQuit :: CInt -> CString -> IO ()
foreign import ccall "get_total_control_threads" cGetTotalControlThreads :: IO CInt

foreign import ccall "main_body_args" cMainBodyArg :: CString -> CString -> CString -> IO ()

foreign import ccall "main_body_body_inside_bool" cMainBodyBodyInsideBool ::
	Ptr CInt -> Ptr Pool -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall "current_pool" cCurrentPool :: IO (Ptr Pool)

foreign import ccall "main_body_body_inside_body_body" cMainBodyBodyInsideBodyBody ::
	CInt -> CInt -> Ptr Pool -> Ptr Work -> IO ()
foreign import ccall "fun_make_work" cMakeWork :: IO (Ptr Work)
foreign import ccall "fun_select_pool" cSelectPool :: CInt -> IO (Ptr Pool)
foreign import ccall "inc_getfail_occasions_and_total_go"
	cIncGetfailOccasionsAndTotalGo :: Ptr Pool -> CInt -> IO ()

data Pool
data Work

foreign import ccall "get_opt_queue" cGetOptQueue :: IO CInt

mainBodyBodyInsideBool :: Ptr CInt -> Ptr Pool -> Ptr CInt -> Ptr CInt -> IO Bool
mainBodyBodyInsideBool ts cp maxStaged lagging =
	(/= 0) <$>  cMainBodyBodyInsideBool ts cp maxStaged lagging

mainBodyBodyInside :: CInt -> CInt -> IO (CInt, CInt)
mainBodyBodyInside v1 v2 = alloca $ \ts -> alloca $ \p1 -> alloca $ \p2 -> do
	poke p1 v1
	poke p2 v2
	cp <- cCurrentPool
	b <- mainBodyBodyInsideBool ts cp p1 p2
	tsv <- peek ts
	v1' <- peek p1
	v2' <- peek p2
	when b $ mainBodyBodyInsideBody tsv cp v1' v2'
	return (v1', v2')

mainBodyBodyInsideBody :: CInt -> Ptr Pool -> CInt -> CInt -> IO ()
mainBodyBodyInsideBody ts cp maxStaged lagging = do
	work <- cMakeWork
	cIncGetfailOccasionsAndTotalGo cp lagging
	pool <- cSelectPool lagging
	cMainBodyBodyInsideBodyBody ts maxStaged pool work

main :: IO ()
main = do
	[addr, nm, pswd] <- getArgs
	putStrLn "begin haskell! delete no arg"
	threadDelay 1000000
	address <- newCString addr
	name <- newCString nm
	password <- newCString pswd

	cCurlGlobalInit cCurlGlobalAll >>= print
	threadDelay 1000000

	cMainBodyArg address name password

	tct <- cGetTotalControlThreads
	when (tct /= 7) . cQuit 1 =<< newCString "bad total control threads"

	initMaxStaged <- cGetOptQueue

	loop (initMaxStaged, 0) (uncurry mainBodyBodyInside)

	cCurlGlobalCleanup

loop :: a -> (a -> IO a) -> IO a
loop x act = do
	r <- act x
	loop r act
