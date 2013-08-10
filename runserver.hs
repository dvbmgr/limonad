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
import Templates
import System.Directory
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad hiding (join)
import qualified Data.Conduit.List
import System.Time
import System.FilePath.Glob
import System.Locale (defaultTimeLocale)

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
getReadMe params = 
	if length matching > 0 then
		renderFileToView [("readme", unsafePerformIO $ readFile $ head matching),
						  ("slug", readGet "n" params)] [] "templates/readme.html"
	else
		renderFileToView [("This repository has no README. Create it in the root directory to be displayed here.", unsafePerformIO $ readFile $ head matching),
						  ("slug", readGet "n" params)] [] "templates/readme.html"
	where
		matching = unsafePerformIO (globDir1 (compile "README*") (rn (gr $ readGet "n" params)))


getLog :: ViewParam -> View
getLog params = renderFileToView [("slug", 
									readGet "n" params)
								] [("commits", 
									(map (toParams) $ reverse (unsafePerformIO $ getChanges (rn $ gr $ readGet "n" params)))
								)] "templates/log.html"
	where
		toParams :: (String, String, String, String, String, String, String, String) -> [(String, String)]
		toParams (id_, cname, message, author, date, log_, files, lines_) = [("id",id_),("cname", cname), ("message", message), ("author", author), ("date", date), ("log", log_), ("files", files), ("lines", lines_)] 

{- TODO -}
getTarBall :: ViewParam -> View
getTarBall params = ViewIO (HttpReturnCode 200) "application/x-tar" Nothing [] (mkTarBall repos_path (gr $ readGet "n" params) >>= readFile)
{- /TODO -}

-------------------------
-- Assets              --
-------------------------
getCSS :: ViewParam -> View
getCSS _ = ViewIO (HttpReturnCode 200) "text/css" Nothing [] (readFile "static/main.css")

getAbout :: ViewParam -> View
getAbout _ = renderFileToView [] [] "templates/about.html"

redirectCode :: ViewParam -> View
redirectCode _ = redirectPermanently "http://github.com/davbaumgartner/DarcsUI"


-------------------------
-- Server              --
-------------------------
main :: IO ()
main = runServer 8080 ([	-- Pages
						Route "GET" "/" getDirectoryList,
						Route "GET" "/readme/" getReadMe,
						Route "GET" "/log/" getLog,
						Route "GET" "/tar/" getTarBall,
							-- Assets
						Route "GET" "/main.css" getCSS,
						Route "GET" "/about" getAbout,
						Route "GET" "/code" redirectCode])