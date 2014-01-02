module Limonad.Types where

	data Variable = Static String String
				  | List [Variable]
				  | Dictionnary String [Variable]
				  deriving (Eq, Show)

	data Env = Env [Variable]
			deriving (Eq, Show)

	lookupV :: Env -> String -> Variable
	lookupV (Env a) s = lookupV' a
		where
			lookupV' ((l@(Static ls _)):xs)
				| ls == s = l
				| otherwise = lookupV' xs
			lookupV' ((l@(Dictionnary ls _)):xs)
				| ls == s = l
				| otherwise = lookupV' xs
			lookupV' [] = error $ "Nothing matching " ++ s ++ " was found !"
			lookupV' (_:xs) = lookupV' xs

	(%+) :: Env -> Env -> Env
	(%+) (Env a) (Env b) = Env (a++b)

	(%:) :: Env -> Variable -> Env 
	(%:) (Env a) v = Env (v:a)

	emptyEnv :: Env
	emptyEnv = Env []