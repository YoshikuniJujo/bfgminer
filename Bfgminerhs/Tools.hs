module Bfgminerhs.Tools (
	loop,
	doWhile,
	doUntil,
	doUntil_,
	while,
	ifM,
	whenM,
	unlessM,
	getCharTimeout,
	waitFor,
	forI
) where

import Control.Monad(when, unless)
import Control.Concurrent

loop :: Monad m => a -> (a -> m a) -> m a
loop x act = do
	r <- act x
	loop r act

doWhile :: Monad m => a -> (a -> m (a, Bool)) -> m a
doWhile x0 action = do
	(x1, b) <- action x0
	if b then doWhile x1 action else return x1

doUntil :: Monad m => a -> (a -> m (a, Bool)) -> m a
doUntil x0 action = do
	(x1, b) <- action x0
	if b then return x1 else doWhile x1 action

doUntil_ :: Monad m => m Bool -> m ()
doUntil_ action = do
	b <- action
	unless b $ doUntil_ action

while :: Monad m => a -> (a -> m Bool) -> (a -> m a) -> m a
while x0 p action = do
	b <- p x0
	if b then do
		x1 <- action x0
		while x1 p action
	else return x0

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p t e = do
	b <- p
	if b then t else e

whenM :: Monad m => m Bool -> m a -> m ()
whenM p t = do
	b <- p
	when b $ t >> return ()

unlessM :: Monad m => m Bool -> m a -> m ()
unlessM p e = do
	b <- p
	unless b $ e >> return ()

getCharTimeout :: Int -> IO (Maybe Char)
getCharTimeout wait = do
	chan <- newChan
	inputThread <- forkIO $ writeChan chan . Just =<< getChar
	_ <- forkIO $ do
		threadDelay (wait * 1000)
		writeChan chan Nothing
		killThread inputThread
	readChan chan

waitFor :: Int -> IO Bool -> IO ()
waitFor sec p = do
	chan <- newChan
	pt <- forkIO $ do
		doUntil_ $ threadDelay 100 >> p
		writeChan chan ()
	_ <- forkIO $ do
		threadDelay $ sec * 1000
		killThread pt
		writeChan chan ()
	readChan chan

forI :: Monad m => Int -> [a] -> (Int -> a -> m b) -> m ()
forI _ [] _ = return ()
forI i (x : xs) action = do
	_ <- action i x
	forI (i + 1) xs action
