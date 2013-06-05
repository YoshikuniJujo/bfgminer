{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import Control.Concurrent
import Foreign.C

foreign import ccall "main_body_no_arg" cMainBodyNoArg :: IO ()
foreign import ccall "main_body_args" cMainBodyArg :: CString -> CString -> CString -> IO ()

main :: IO ()
main = do
	[addr, nm, pswd] <- getArgs
	putStrLn "begin haskell!"
	threadDelay 1000000
	address <- newCString addr
	name <- newCString nm
	password <- newCString pswd
	cMainBodyArg address name password
