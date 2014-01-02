module Limonad.Templates.Eval (evaluate, evalT) where

    import Limonad.Templates.Types
    import Limonad.Templates.Parser
    import qualified Data.String.Utils as U
    import Data.Functor

    lookupEnv env n =
    	if ln == 1 then
    		lookupV env n
	    else
	    	lookupV (Env (case lookupV env $ head n' of
	    							Dictionnary _ l -> l
	    							otherwise -> error "Something went wrong…")) (head $ drop 1 n') 
    	where
    		n' = U.split "." n
    		ln = length n'

    evaluate :: Env -> [Template] -> IO String
    evaluate env templates = do
    	v <- sequence $ map (evalT env) templates
    	return $ U.join "" v 

    evalT :: Env -> Template -> IO String
    evalT env (Var n) =
    	case lookupEnv env n of 
    		Static _ v ->
    			return v
    		v ->
    			error "Showing a variable requires it to be a Static"
    evalT env@(Env e) (For o n t) =
    	let
    		v = case lookupEnv env o of
    				Dictionnary _ r -> r
    				_ -> error $ "Unvalid dict"
    	in
    		(U.join "" <$> sequence [evaluate en t | l <- v, (case l of 
    																Dictionnary _ _ -> True
    																List _ -> True
    																_ -> False), 
    														let en = case l of 
    															Dictionnary _ c ->
    																Env ((Dictionnary n c):e)
    															List c ->
    																Env ((Dictionnary n c):e)
    															_ -> env])
    evalT _ (Comment s) =
    		return $ "<!--[TEMPLATE " ++ s ++ "]-->"
    evalT env (Include p) = 
    		(readFile p) >>= evaluate env . parseTemplate 
    evalT _ (Aside s) = 
    		return s