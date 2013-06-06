module Bfgminerhs.Tools (
	loop,
	doWhile
) where

import Control.Monad (when)

loop :: a -> (a -> IO a) -> IO a
loop x act = do
	r <- act x
	loop r act

doWhile :: a -> (a -> IO (a, Bool)) -> IO ()
doWhile x0 action = do
	(x1, b) <- action x0
	when b $ doWhile x1 action
