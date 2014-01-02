import Limonad.Templates.Shortcuts
import Limonad.Templates.Types
import Limonad.Types

env = emptyEnv %:
				Static "coucou" "prout"
			   %:
				Static "hello" "blabla"
			   %:
			   	Dictionnary "patate" [List [Dictionnary "frite" [
								  					List [Static "tr" "de"],
								  					List [Static "tr" "pain"]]
								  		, Static "lorem" "ipsum"],
								  List [Dictionnary "frite" [
								  					List [Static "tr" "op"]]
								  		, Static "lorem" "dolor"]
								  ]

main :: IO ()
main = do
	renderFile env "main.tpl" >>= putStrLn