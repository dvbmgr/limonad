module Limonad.Templates.Parser (parseTemplate) where

	import Limonad.Templates.Types
	import Text.ParserCombinators.Parsec
	import Control.Applicative hiding (many, (<|>))
	import Data.Functor

	parseTemplate :: String -> [Template]
	parseTemplate input = case parse parseAll "template error" input of
		Right template -> template
		Left err -> error $ show err

	parseAll :: Parser [Template]
	parseAll = many (try parseVariable <|> try parseFor <|> try parseComment <|> try parseInclude <|> parseAside)

	parseVariable :: Parser Template
	parseVariable = do
		string "{{" <?> "tag begin"
		spaces
		id <- many1 (alphaNum <|> char '.')
		spaces
		string "}}" <?> "tag end"
		return $ Var id

	parseFor :: Parser Template
	parseFor = do
		string "{%" <?> "call begin"
		spaces
		string "for" <?> "for"
		spaces
		replace <- many1 (alphaNum <|> char '.')
		spaces		
		string "in" <?> "in"
		spaces
		basename <- many1 (alphaNum <|> char '.')
		spaces
		content <- between (string "%}") (string "{%") parseAll
		spaces 
		string "endfor" <?> "endfor"
		spaces
		string "%}" <?> "call end"
		return $ For basename replace content

	parseComment :: Parser Template
	parseComment = do
		string "{#" <?> "comment"
		spaces
		content <- many1 (noneOf "#" <|> try (char '#' <* notFollowedBy (char '}')))
		spaces
		string "#}" <?> "end of comment"
		return $ Comment content

	parseInclude :: Parser Template
	parseInclude = do
		string "{%" <?> "call begin"
		spaces
		string "include" <?> "include"
		spaces
		path <- many1 (alphaNum <|> char '.' <|> char '/' <|> char '\\')
		spaces
		string "%}" <?> "call end"
		return $ Include path

	parseAside :: Parser Template
	parseAside = do
		content <- many1 (noneOf "{" <|> try (char '{' <* notFollowedBy (char '{' <|> char '%' <|> char '#')))
		return $ Aside content