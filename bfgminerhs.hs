{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bfgminerhs.Foreign
import Bfgminerhs.Tools

import System.Environment (getProgName, getArgs)
import Control.Concurrent (threadDelay)
import Control.Monad (when)

mainInsideBodyBody :: Int -> Int -> Pool -> Work -> IO ()
mainInsideBodyBody ts maxStaged pool@(Pool ppool) work@(Work pwork) = do
	hasStratum <- doesPoolHaveStratum pool
	if hasStratum then poolHasStratum pool work else do
		retry <- poolNotHasStratum ts maxStaged pool work
		when retry $ mainInsideBodyBody ts maxStaged pool work

mainInsideBody :: Int -> Pool -> Int -> Bool -> IO ()
mainInsideBody ts cp maxStaged lagging = do
	work <- makeWork
	incGetfailOccasionsAndTotalGo cp lagging
	pool <- selectPool lagging
	mainInsideBodyBody ts maxStaged pool work

main :: IO ()
main = do
	progName <- getProgName
	args <- getArgs
	putStrLn "begin haskell! delete no arg"

	curlGlobalInit curlGlobalAll >>= print
	threadDelay 1000000

	mainInitialize $ progName : args

	tct <- getTotalControlThreads
	when (tct /= 7) $ quit 1 "bad total control threads"

	initMaxStaged <- getOptQueue

	loop (initMaxStaged, False) $ \(maxStaged, lagging) -> do
		cp <- currentPool
		(b, (ts, maxStaged', lagging')) <- mainInsideBool cp maxStaged lagging
		when b $ mainInsideBody ts cp maxStaged' lagging'
		return (maxStaged', lagging')

	curlGlobalCleanup
