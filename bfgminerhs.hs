{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import Control.Concurrent
import Foreign.C

foreign import ccall "main_body_no_arg" cMainBodyNoArg :: IO ()
foreign import ccall "main_body_args" cMainBodyArg :: CString -> CString -> CString -> IO ()

main :: IO ()
main = do
	putStrLn "begin haskell!"
	threadDelay 1000000
	address <- newCString "pool.50btc.com:8332"
	name <- newCString "PAF01143@nifty.ne.jp_powerfull"
	password <- newCString "hoge"
	cMainBodyArg address name password
