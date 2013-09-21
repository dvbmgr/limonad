-- Copyright (c) 2013, David Baumgartner <ch.davidbaumgartner@gmail.com>
-- 
-- All rights reserved.
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
 
-- * Redistributions of source code must retain the above copyright
--   notice, this list of conditions and the following disclaimer.
-- * Redistributions in binary form must reproduce the above copyright
--   notice, this list of conditions and the following disclaimer in the
--   documentation and/or other materials provided with the distribution.
-- * Neither the name of the David Baumgartner nor the
--   names of its contributors may be used to endorse or promote products
--   derived from this software without specific prior written permission.

-- THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND ANY
-- EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE REGENTS AND CONTRIBUTORS BE LIABLE FOR ANY
-- DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
-- (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
-- LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
-- ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

import Darcs.Commands
import DarcsAPI
import GHC.IO.Handle
import System.IO
import Server
import Mime
import Templates
import System.Process
import System.Directory
import System.Posix.Files

import Data.String.Utils
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad hiding (join)
import qualified Data.Conduit.List
import System.Time
import System.FilePath.Glob
import System.Locale (defaultTimeLocale)
import Data.Functor ((<$>))

import Text.Markdown
import Text.Blaze.Html.Renderer.Text
import qualified Data.Text.Lazy as T
import qualified Data.ByteString.Lazy as BS

import Data.MIME.Types

-------------------------
-- Configuration       --
-------------------------
-- Repository path
repos_path :: String
repos_path = "./repos"

-------------------------
-- Helpers             --
-------------------------
-- Get computed name
rn :: String -> String 
rn name = repos_path ++ "/" ++ name

-- Slug -> Path
gr :: String -> String
gr slug = if length nv == 1 then 
				head $ nv
			else
				error "Not found"
	where
		matching b = [fst a | a <- b, snd a == slug] 
		nv = matching $ map (\a -> (a, slugifyString a)) (unsafePerformIO $ getDirectoryContents repos_path)


renderMarkdown :: String -> String
renderMarkdown = replace "'" "&rsquo;" . replace "..." "&hellip;" . replace "--" "&ndash;" . replace "---" "&mdash;" . T.unpack . renderHtml . markdown def . T.pack

toParams :: (String, String, String, String, String, String, String, String, String) -> [(String, String)]
toParams (id_, cname, message, author, date, age, log_, files, lines_) = [("id",id_),("cname", cname), ("message", message), ("author", author), ("date", date), ("age", age), ("log", log_), ("files", files), ("lines", lines_)] 


-------------------------
-- Pages               --
-------------------------
getDirectoryList :: ViewParam -> View
getDirectoryList params = basicViewIO (getDirectoryContents repos_path >>= \repos -> renderFile [] [("repositories", (reps repos))] "templates/index.html")
	where
		reps :: [FilePath] -> [[(String,String)]]
		reps = map (\a -> [	("name",	a),
							("slug",	slugifyString a),
							("desc",	unIOString $ showTags $ rn a),
							("author",	unIOString $ showAuthor $ rn a),
							("date",	unIOString $ showAge $ rn a),
							("commits",	show $ unsafePerformIO $ showCommitNth $ rn a)
						]) . filter (\a -> ((a !! 0) /= '.'))

getReadMe :: ViewParam -> View
getReadMe params = basicViewIO (do
		matching <- (globDir1 (compile "README*") (rn name))
		readm <- readFile $ if length matching > 0 then head matching else "static/noreadme.md"
		fl <- renderFile [("readme", renderMarkdown readm),
								  		("slug", readGet "n" params), ("repo", name)] [] "templates/readme.html"
		return fl)
	where
		name = gr $ readGet "n" params

getLog :: ViewParam -> View
getLog params = basicViewIO (do
		changes <- getChanges (rn $ gr $ readGet "n" params)
		fl <- renderFile [("slug", 
									readGet "n" params), ("repo", gr $ readGet "n" params)
								] [("commits", 
									(map (toParams) $ reverse changes)
								)] "templates/log.html"
		return fl)

getCommit :: ViewParam -> View
getCommit params = basicViewIO (do
	pr <- getDiff (rn $ gr $ readGet "n" params) (readGet "h" params)
	csdetails <- getChanges (rn $ gr $ readGet "n" params)
	fl <- renderFile ([("slug", 
						readGet "n" params), ("repo", gr $ readGet "n" params),
					  ("diff", unlines $ drop 2 $ lines pr)
					]++(toParams $ head $ filter (\(a,_,_,_,_,_,_,_,_) -> (a == readGet "h" params)) csdetails)) [] "templates/commit.html"
	return fl)

getTree :: ViewParam -> View
getTree params = basicViewIO (do
		let path = ((rn $ gr $ readGet "n" params) ++ "/" ++ readGet "p" params)
		fs <- getFileStatus path
		fl <- (if isDirectory fs then do
					dl <- (getDirectoryContents path)
					fl <- renderFile [("slug", readGet "n" params), ("repo", gr $ readGet "n" params)] [("files", (map (\a -> [("name", a)]) $ filter (\a -> a /= "_darcs" && a !! 0 /= '.') dl))] "templates/tree.html"
					return fl
				else do
					ct <- readFile path
					fl <- renderFile [("slug", readGet "n" params), ("repo", gr $ readGet "n" params), ("file", ct), ("mime", searchFor path)] [] "templates/file.html" 
					return fl)
		return fl
	)

getDownloadFile :: ViewParam -> View
getDownloadFile params = ViewIOBS (HttpReturnCode 200) (searchFor $ readGet "f" params) Nothing [] (do 
		fil <- BS.readFile $ (rn $ gr $ readGet "n" params) ++ "/" ++ (replace ".." "." $ replace "//" "/" $ readGet "f" params)
		return fil
	)

getTarBall :: ViewParam -> View
getTarBall params = ViewIOBS (HttpReturnCode 200) "application/x-tar" Nothing [("Content-Disposition","attachment; filename=\""++(gr $ readGet "n" params)++".tar.gz\"")] (do
		cur <- getCurrentDirectory
		setCurrentDirectory $ rn $ gr $ readGet "n" params
		_ <- readProcessWithExitCode "darcs" ["dist"] []
		setCurrentDirectory cur
		fil <- BS.readFile $ (rn $ gr $ readGet "n" params) ++ "/" ++ (gr $ readGet "n" params) ++ ".tar.gz"
		return fil
	)

-------------------------
-- Assets              --
-------------------------
getStatic :: String -> ViewParam -> View
getStatic file _ = ViewIOBS (HttpReturnCode 200) "text/css" Nothing [] (BS.readFile file)

getAbout :: ViewParam -> View
getAbout _ = renderFileToView [] [] "templates/about.html"

redirectCode :: ViewParam -> View
redirectCode _ = redirectPermanently "http://github.com/davbaumgartner/DarcsUI"

-------------------------
-- Server              --
-------------------------
main :: IO ()
main = do
	codeMirrorLanguages <- getDirectoryContents "static/codemirror/"
	runServer 8080 ([	-- Pages
						Route "GET" "/" getDirectoryList,
						Route "GET" "/readme/" getReadMe,
						Route "GET" "/log/" getLog,
						Route "GET" "/commit/" getCommit,
						Route "GET" "/tree/" getTree,
						Route "GET" "/download/" getDownloadFile,
						Route "GET" "/tar/" getTarBall,
							-- CodeMirror
						Route "GET" "/codemirror/monokai.css" (getStatic "static/codemirror/theme.monokai.css"),
							-- Assets
						Route "GET" "/limonad.css" (getStatic "static/main.css"),
						Route "GET" "/about" getAbout,
						Route "GET" "/code" redirectCode]++
							-- CodeMirror
						map (\a -> Route "GET" ("/codemirror/" ++ (join "." $ drop 1 $ split "." a)) (getStatic ("static/codemirror/" ++ a))) (filter (startswith "code.") codeMirrorLanguages)
						)