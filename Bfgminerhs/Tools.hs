module Bfgminerhs.Tools (
	loop,
	doWhile
) where

import Control.Monad (when)

loop :: a -> (a -> IO a) -> IO a
loop x act = do
	r <- act x
	loop r act

doWhile :: IO Bool -> IO ()
doWhile action = do
	b <- action
	when b $ doWhile action
