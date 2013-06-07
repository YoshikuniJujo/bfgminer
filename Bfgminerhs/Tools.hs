module Bfgminerhs.Tools (
	loop,
	doWhile,
	while
) where

loop :: a -> (a -> IO a) -> IO a
loop x act = do
	r <- act x
	loop r act

doWhile :: a -> (a -> IO (a, Bool)) -> IO a
doWhile x0 action = do
	(x1, b) <- action x0
	if b then doWhile x1 action else return x1

while :: a -> (a -> IO Bool) -> (a -> IO a) -> IO a
while x0 p action = do
	b <- p x0
	if b then do
		x1 <- action x0
		while x1 p action
	else return x0
