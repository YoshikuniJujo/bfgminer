module Bfgminerhs.Tools (
	loop,
	doWhile,
	while,
	ifM,
	whenM,
	unlessM
) where

import Control.Monad(when, unless)

loop :: Monad m => a -> (a -> m a) -> m a
loop x act = do
	r <- act x
	loop r act

doWhile :: Monad m => a -> (a -> m (a, Bool)) -> m a
doWhile x0 action = do
	(x1, b) <- action x0
	if b then doWhile x1 action else return x1

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
