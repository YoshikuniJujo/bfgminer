{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import Control.Concurrent
import Foreign.C

foreign import ccall "curl_global_init" cCurlGlobalInit :: CInt -> IO CInt
foreign import ccall "curl_global_cleanup" cCurlGlobalCleanup :: IO ()
foreign import ccall "curl_global_all" cCurlGlobalAll :: CInt

foreign import ccall "main_body_args" cMainBodyArg :: CString -> CString -> CString -> IO ()
foreign import ccall "main_body_body" cMainBodyBody :: IO ()

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
	cMainBodyBody

	cCurlGlobalCleanup
