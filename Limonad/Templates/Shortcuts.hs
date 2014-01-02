module Limonad.Templates.Shortcuts where

	import Limonad.Templates.Types
	import Limonad.Templates.Parser
	import Limonad.Templates.Eval

	renderString :: Env -> String -> IO String
	renderString env = evaluate env . parseTemplate

	renderFile :: Env -> FilePath -> IO String
	renderFile env f = readFile f >>= renderString env