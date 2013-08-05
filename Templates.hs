module Templates (renderString, renderFile, renderFileToView, renderStringToView, basePage, basePage') where
	import Server
	import Text.ParserCombinators.Parsec hiding (spaces)
	import Control.Applicative hiding (many, (<|>))
	import System.Environment
	import Control.Monad
	import System.IO.Unsafe (unsafeDupablePerformIO)
	import qualified Data.String.Utils as SUtils

	type LocalVars = [(String, String)]

	type DBVars = [(String, [[(String, String)]])]

	data Template = Variable LocalVars DBVars String
				  | HTML LocalVars DBVars String
				  | DBVariable LocalVars DBVars String String String
				  | Include LocalVars DBVars String

	instance Show Template where show = showParsed

	readLocalVars :: LocalVars -> String -> LocalVars
	readLocalVars lvars name = [l|l<-lvars, (fst l)==name]

	readDBVars :: DBVars -> String -> DBVars
	readDBVars dvars name = [l|l<-dvars, (fst l)==name]

	readExpr :: LocalVars -> DBVars -> String -> String
	readExpr lvars dvars input = case parse (parseAll lvars dvars) "" input of
	    Left err -> "[ERROR] " ++ show err
	    Right val -> concatMap showParsed val

	parseAll :: LocalVars -> DBVars -> Parser [Template]
	parseAll lvars dvars = many ((parseVariable lvars dvars) <|> (parseInclude lvars dvars) <|> (parseDBVariable lvars dvars) <|> (parseHTML lvars dvars))

	parseHTML :: LocalVars -> DBVars -> Parser Template
	parseHTML lvars dvars = HTML lvars dvars <$> many1 (noneOf "{[!" <|> try (char '{' <* notFollowedBy (char '{')) <|> try (char '[' <* notFollowedBy (char '[')) <|> try (char '!' <* notFollowedBy (char '(')))

	parseVariable :: LocalVars -> DBVars -> Parser Template
	parseVariable lvars dvars = Variable lvars dvars <$> (string "{{ " *> many (oneOf (['a'..'z']++['A'..'Z'])) <* string " }}")

	parseInclude :: LocalVars -> DBVars -> Parser Template
	parseInclude lvars dvars = do
		_ <- string "!("
		filename <- many (oneOf (['a'..'z']++['A'..'Z']++['0'..'9']++['.','/','_']))
		_ <- string ")"
		return (Include lvars dvars filename)

	parseDBVariable :: LocalVars -> DBVars -> Parser Template
	parseDBVariable lvars dvars = do
		_ <- string "[[ "
		originalname <- many (oneOf (['a'..'z']++['A'..'Z']))
		_ <- string " as "
		newname <- many (oneOf (['a'..'z']++['A'..'Z']))
		_ <- string " ]]("
		content <- many (noneOf ")")
		_ <- string ")"
		return $ DBVariable lvars dvars originalname newname content

	showParsed :: Template -> String
	showParsed (DBVariable lvars dvars originalname newname content) = 
		SUtils.join "" [parseLocal a|a<-cvar]
		where 
			parseLocal :: [(String, String)] -> String
			parseLocal var = 
				parseLocal' var content
				where
					newstring :: String
					newstring = 
						SUtils.replace originalname newname content
					mkname :: (String, String) -> String	
					mkname rvar =
						"{{ "++(SUtils.join "." [newname, fst rvar])++" }}"
					parseLocal' :: [(String, String)] -> String -> String
					parseLocal' [] str = 
						str
					parseLocal' (cvar:ovar) str = 
						parseLocal' ovar (SUtils.replace (mkname cvar) (snd cvar) str)
			cvar :: [[(String, String)]]
			cvar = 
				if length match > 0 then
					snd (match !! 0)
				else
					error ("No db variable matching ``" ++ originalname ++ "'' is defined")
				where 
					match = readDBVars dvars originalname
	showParsed (HTML lvars dvars x) = x
	showParsed (Variable lvars dvars x) = 
		if length matching > 0 then
			snd $ matching !! 0
		else
			error ("No variable matching ``" ++ x ++ "'' is defined")
		where
			matching = readLocalVars lvars x
	showParsed (Include lvars dvars x) = "<!-- Templates' includes are DANGEROUS ! Use it at you own risks -->" ++ (unsafeDupablePerformIO $ readFile x)

	-- Read an parse from string
	renderString :: LocalVars -> DBVars -> String -> String
	renderString lvars dvars string = readExpr lvars dvars string

	-- Read an parse from file
	renderFile :: LocalVars -> DBVars -> String -> IO String
	renderFile lvars dvars fname = do
		content <- readFile fname
		return $ renderString lvars dvars content

	renderFileToView :: LocalVars -> DBVars -> String -> View 
	renderFileToView lvars dvars fname = basicViewIO (renderFile lvars dvars fname)

	renderStringToView :: LocalVars -> DBVars -> String -> View 
	renderStringToView lvars dvars string = basicView (renderString lvars dvars string)

	basePage :: [String] -> [String] -> String -> String -> String
	basePage csss jss title content = 
		"<!doctype html><html><head><meta charset='utf-8'/><title>"++title ++"</title>"++
			(SUtils.join "" ["<link href='"++x++"' rel='stylesheet' />"|x<-csss])++
			(SUtils.join "" ["<script src='"++x++"' type='text/javascript'></script>"|x<-jss])++
		"</head><body>"++
			content ++
		"</body></html>"

	basePage' :: String -> String -> String
	basePage' title content  = basePage [] [] title content