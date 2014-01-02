module Limonad.Server.Types where

	import Network.Socket (PortNumber, HostAddress)
	import System.Directory
	import Limonad.Interface.Types

	data Config = Config {
			getPort :: PortNumber,
			getPhysicalRoot :: String,
			getLogicalRoot :: String,
			getRoutes :: Routes
		}

	defaultConfig :: IO Config
	defaultConfig = do 
		getCurrentDirectory >>= \pwd ->
					Config {
						getPort = PortNumber 3000,
						getPhysicalRoot = pwd,
						getLogicalRoot = "/",
						getRoutes = [],
					}