module Limonad.Templates.Types (Variable(..), Template(..), Env(..), lookupV) where

	import Limonad.Types

	data Template = Var String
				  | For String String [Template]
				  | Comment String
				  | Include String
				  | Aside String
				  deriving (Eq, Show)