module Limonad.Markdown.Parser where

	import Limonad.Markdown.Types
	import Text.ParserCombinators.Parsec
	import Control.Applicative hiding (many, (<|>))
	import Data.Functor

	dnl = newline >> newline

	parseMarkdown :: String -> [Markdown]
	parseMarkdown input = case parse parseAll "markdown error" input of
		Right template -> template
		Left err -> error $ show err

	parseAll :: Parser [Markdown]
	parseAll = many (try parseHeaders <|> try parseQuote <|> try parseStrongEmphasis <|> parseEmphasis)

	parseHeaders :: Parser Markdown
	parseHeaders = try (mkParserHeader 1 Header1) <|> try (mkParserHeader 2 Header2) <|> try (mkParserHeader 3 Header3) <|> try (mkParserHeader 4 Header4) <|> try (mkParserHeader 5 Header5) <|> try (mkParserHeader 6 Header6) <|> try parseHeader1F <|> parseHeader2F

	parseHeader1F :: Parser Markdown
	parseHeader1F = do
		title <- many1 alphaNum
		many (char '=')
		return $ Header1 title

	parseHeader2F :: Parser Markdown
	parseHeader2F = do
		title <- many1 alphaNum
		many (char '-')
		return $ Header1 title

	mkParserHeader :: Int -> (String -> Markdown) -> Parser Markdown
	mkParserHeader cnt t = do
		string (take cnt $ repeat '#')
		spaces 
		title <- many1 alphaNum
		try $ string (take cnt $ repeat '#')
		return $ t title

	parseQuote :: Parser Markdown
	parseQuote = do
		c <- manyTill (char '>' <* anyChar) dnl
		case (parse parseAll "markdown error" $ concat $ map (drop 1) $ lines c) of 
			Right a -> return $ Quote a
			Left e -> error $ show e

	parseParagraph :: Parser Markdown
	parseParagraph = do
		c <- manyTill anyToken dnl
		case (parse parseAll "markdown error" c) of 
			Right a -> return $ Paragraph a
			Left e -> error $ show e

	parseEmphasis :: Parser Markdown
	parseEmphasis = do
		t <- try (between (char '*') (char '*') anyChar) <|> (between (char '_') (char '_') anyChar)
		return $ Emphasis [t]

	parseStrongEmphasis :: Parser Markdown
	parseStrongEmphasis = do
		t <- try (between (string "**") (string "**") anyChar) <|> (between (string "__") (string "__") anyChar)
		return $ StrongEmphasis [t]

