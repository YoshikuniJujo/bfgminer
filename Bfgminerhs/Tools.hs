module Bfgminerhs.Tools (
	loop
) where

loop :: a -> (a -> IO a) -> IO a
loop x act = do
	r <- act x
	loop r act
