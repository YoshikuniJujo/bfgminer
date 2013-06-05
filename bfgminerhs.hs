{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import Control.Concurrent
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

foreign import ccall "main_body_body_inside" cMainBodyBodyInside ::
	Ptr CInt -> Ptr CInt -> IO ()

foreign import ccall "get_opt_queue" cGetOptQueue :: IO CInt

mainBodyBodyInside :: CInt -> CInt -> IO (CInt, CInt)
mainBodyBodyInside v1 v2 = alloca $ \p1 -> alloca $ \p2 -> do
	poke p1 v1
	poke p2 v2
	cMainBodyBodyInside p1 p2
	v1' <- peek p1
	v2' <- peek p2
	return (v1', v2')

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
