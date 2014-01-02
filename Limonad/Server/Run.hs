{-# LANGUAGE OverloadedStrings #-}
module Limonad.Server.Run (run) where

	import Limonad.Server.Types
	import Limonad.Utils
	import qualified Data.ByteString as B
	import Network hiding (accept)
	import Network.Socket
	import Network.Socket.ByteString (sendAll)
	import Control.Concurrent

	send :: View -> IO ()

	handle :: Socket -> IO ()
	handle sock = do
		sendAll	

	loop sock = accept sock >>= 
					\(conn, addr) -> 
						forkIO $ handle sock >> loop sock

	run :: Config -> IO ()
	run config = 
		withSocketsDo (listenOn $ getPort config >>= loop)
