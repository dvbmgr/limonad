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


module Server (	runServer, 
				Routes, 
				Route(..), 
				View(..), 
				HttpReturnCode(..), 
				ViewParam(..), 
				URI(..), 
				basicView, 
				basicViewIO, 
				readGet, 
				readPost, 
				slugifyString, 
				readFromUnicode, 
				redirectPermanently, 
				redirectTemporary,
				unIOString) where

	import Data.Time
	import Data.Char
	import Data.String.Utils (join, split, strip)
	import Network.Socket
	import Control.Monad hiding (join)
	import Control.Concurrent
	import System.IO.Unsafe (unsafeDupablePerformIO)
	import Data.String.Unicode

	data HttpReturnCode = HttpReturnCode Int
		deriving (Read, Eq)

	data URIAuth = URIAuth {
			uriUser :: String,
			uriPassword :: String
		}
		deriving (Read, Show)

	data URI = URI {
			uriScheme :: String,
			uriAuth :: Maybe URIAuth,
			uriHost :: String,
			uriPort :: Maybe Int,
			uriPath :: String,
			uriQuery :: [(String,String)],
			uriFragment :: String
		}
		deriving (Read, Show)

	data ViewParam = ViewParam {
			paramMethod :: String,
			paramUri :: URI,
			paramPost :: [(String,String)]
		}
		deriving (Read, Show)

	data View = ViewIO {
			viewIOReturnCode :: HttpReturnCode,
			viewIOContentType :: String,
			viewIOExpires :: Maybe String,
			viewIOAdditionnalHeaders :: [(String,String)],
			viewIOContent :: IO String
		} | View {
			viewReturnCode :: HttpReturnCode,
			viewContentType :: String,
			viewExpires :: Maybe String,
			viewAdditionnalHeaders :: [(String,String)],
			viewContent :: String
		}

	data Route = Route {
			routeMethod :: String,
			routePath :: String,
			routeFunct :: (ViewParam -> View)
		}

	type Routes = [Route]


	instance Show HttpReturnCode where
		-- Informational
		show (HttpReturnCode 100) = "100 Continue"
		show (HttpReturnCode 101) = "101 Switching Protocols"
		-- Successful
		show (HttpReturnCode 200) = "200 OK"
		show (HttpReturnCode 201) = "201 Created"
		show (HttpReturnCode 202) = "202 Accepted"
		show (HttpReturnCode 203) = "203 Non-Authoritative Information"
		show (HttpReturnCode 204) = "204 No Content"
		show (HttpReturnCode 205) = "205 Reset Content"
		show (HttpReturnCode 206) = "206 Partial Content"
		-- Redirection
		show (HttpReturnCode 300) = "300 Multiple Choices"
		show (HttpReturnCode 301) = "301 Moved Permanently"
		show (HttpReturnCode 302) = "302 Found"
		show (HttpReturnCode 303) = "303 See Other"
		show (HttpReturnCode 304) = "304 Not Modified"
		show (HttpReturnCode 305) = "305 Use Proxy"
		--show 306 = Unused
		show (HttpReturnCode 307) = "307 Temporary Redirect"
		-- Client error
		show (HttpReturnCode 400) = "400 Bad request"
		show (HttpReturnCode 401) = "401 Unauthorized"
		show (HttpReturnCode 402) = "402 Payment Required"
		show (HttpReturnCode 403) = "403 Forbidden"
		show (HttpReturnCode 404) = "404 Not Found"
		show (HttpReturnCode 405) = "405 Method Not Allowed"
		show (HttpReturnCode 406) = "406 Not Acceptable"
		show (HttpReturnCode 407) = "407 Proxy Authentication Required"
		show (HttpReturnCode 408) = "408 Request Timeout"
		show (HttpReturnCode 409) = "409 Conflict"
		show (HttpReturnCode 410) = "410 Gone"
		show (HttpReturnCode 411) = "411 Length Required"
		show (HttpReturnCode 412) = "412 Precondition Failed"
		show (HttpReturnCode 413) = "413 Request Entity Too Large"
		show (HttpReturnCode 414) = "414 Request-URI Too Long"
		show (HttpReturnCode 415) = "415 Unsupported Media Type"
		show (HttpReturnCode 416) = "416 Requested Range Not Satisfiable"
		show (HttpReturnCode 417) = "417 Expectation Failed"
		show (HttpReturnCode 418) = "418 I'm a teapot"
		-- Server error
		show (HttpReturnCode 500) = "500 Internal Server Error"
		show (HttpReturnCode 501) = "501 Not Implemented"
		show (HttpReturnCode 502) = "502 Bad Gateway"
		show (HttpReturnCode 503) = "503 Service Unavailable"
		show (HttpReturnCode 504) = "504 Gateway Timeout"
		show (HttpReturnCode 505) = "505 HTTP Version Not Supported"
		-- Other
		show _ = show (HttpReturnCode 500)

	to_tuple :: [a] -> (a,a)
	to_tuple (b:c:d) = (b,c)

	runServer :: PortNumber -> Routes -> IO ()
	runServer port routes = do
		sock <- socket AF_INET Stream 0
		setSocketOption sock ReuseAddr 1 
		bindSocket sock (SockAddrInet port iNADDR_ANY)
		listen sock 10
		putStrLn ("Starting server on 0.0.0.0:" ++ show port)
		loopServer routes sock

	loopServer :: Routes -> Socket -> IO ()
	loopServer routes sock = do
		accept sock >>= \conn -> forkIO (handleRequest routes conn)
		loopServer routes sock

	handleRequest :: Routes -> (Socket, SockAddr) -> IO ()
	handleRequest routes (sock, addr) = do
		datas <- recv sock 4096
		let http = parseHttp datas
		let view = handleHttp routes http
		putStrLn ("["++(show addr)++"] " ++ (uriPath $ paramUri http))
		sendSock sock view
		sClose sock

	sendSock :: Socket -> View -> IO Int
	sendSock sock (ViewIO returncode contenttype expires additionnalheaders content) = liftM (viewToResponse returncode contenttype expires additionnalheaders) content >>= send sock
	sendSock sock (View returncode contenttype expires additionnalheaders content) = send sock (viewToResponse returncode contenttype expires additionnalheaders content)

	parseHttp :: String -> ViewParam
	parseHttp request = (ViewParam method (URI "http" Nothing ([split ":" (snd x) !! 0 | x <- httpparams, fst x == "Host"] !! 0) port onlypath getparams fragement) postparams)
		where
			method :: String
			method = words ((lines request) !! 0) !! 0
			httpparams :: [(String,String)]
			httpparams = map (to_tuple . split ": ") (listhttpparams [] (tail $ lines request))
			listhttpparams :: [String] -> [String] -> [String]
			listhttpparams out (x:xs) = 
				if strip x == "" then
					out
				else
					listhttpparams (x:out) xs
			listhttpparams a b = error (join "\n" a ++ join "\n" b)
			port :: Maybe Int
			port = ([if length (split ":" (snd x)) > 1 then (Just (read ((split ":" (snd x)) !! 1)::Int)) else Nothing | x <- httpparams, fst x == "Host"] !! 0)
			getparams :: [(String,String)]
			getparams = 
				if length pr > 1 then
					map (\x -> if length (split "=" x) > 1 then to_tuple (split "=" x) else (x,""))  (split "&" (pr !! 1))
				else
					[]
				where 
					pr = (split "?" ((split "#" path) !! 0))
			postparams :: [(String,String)]
			postparams = map (\x -> if length (split "=" x) > 1 then to_tuple (split "=" x) else (x,""))  (split "&" pr)
				where 
					pr = strip $ brutpostparams (lines request)

			brutpostparams :: [String] -> String
			brutpostparams xs = 
				if (strip $ last xs) /= "" then
					last xs
				else
					"nothing=nothing"
			onlypath :: String
			onlypath = (split "?" ((split "#" path) !! 0)) !! 0 
			path :: String
			path = words ((lines request) !! 0) !! 1
			fragement :: String
			fragement = "#" ++ (split "#" path) !! 1

	handleHttp :: Routes -> ViewParam -> View
	handleHttp routes params = 
			if length matching == 1 then
				(matching !! 0) params
			else
				if length e4040 == 1 then
					(e4040 !! 0) params
				else
					(View (HttpReturnCode 500) "text/html" Nothing [] "<h1>Internal Server Error</h1>")
		where 
			matching = [routeFunct route|route <- routes, routePath route == (uriPath (paramUri params)), (routeMethod route == paramMethod params) || (routeMethod route == "*")]
			e4040 = [routeFunct route|route <- routes, routePath route == "/404"]

	basicView :: String -> View 
	basicView content = View (HttpReturnCode 200) "text/html" Nothing [] content

	basicViewIO :: IO String -> View 
	basicViewIO content = ViewIO (HttpReturnCode 200) "text/html" Nothing [] content

	readGet :: String -> ViewParam -> String
	readGet search params = 
		if length found > 0 then
			found !! 0
		else
			""
		where
			found = [snd param | param <- (uriQuery $ paramUri params), fst param == search] 
	readPost :: String -> ViewParam -> String
	readPost search params = 
		if length found > 0 then
			found !! 0
		else
			""
		where
			found = [snd param | param <- (paramPost params), fst param == search] 

	slugifyString :: String -> String
	slugifyString xp@(x:xs) = 
		if x `elem` (['a'..'z']++['A'..'Z']++['0'..'9']) then 
			(toLower x):(slugifyString xs)
		else
			'-':(slugifyString xs)
	slugifyString _ = ""

	unIOString :: IO String -> String 
	unIOString = readFromUnicode . unsafeDupablePerformIO

	readFromUnicode :: String -> String
	readFromUnicode = unicodeRemoveNoneAscii . latin1ToUnicode

	redirectTemporary :: String -> View 
	redirectTemporary url = View (HttpReturnCode 302) "text/html" Nothing [("Location",url)] ("Document has moved temporary <a href='"++url++"'>here</a>.")

	redirectPermanently :: String -> View 
	redirectPermanently url = View (HttpReturnCode 301) "text/html" Nothing [("Location",url)] ("Document has moved permanently <a href='"++url++"'>here</a>.")

	viewToResponse :: HttpReturnCode -> String -> Maybe String -> [(String,String)] -> String -> String
	viewToResponse returncode contenttype expires additionnalheaders content = "HTTP/1.0 " ++ show returncode ++ "\n" ++
																					"Server: Darcsinter" ++ "\n" ++
																					"Transfer-Encoding: chuncked" ++ "\n" ++
 																					"Connection: keep-alive" ++ "\n" ++
 																					"Content-type: " ++ contenttype ++ (if (length additionnalheaders) > 0 then "\n" else "")++
 																					(join "\n" [fst a ++ ": " ++ snd a| a <- additionnalheaders]) ++ "\n" ++
																					"Content-length: " ++ show (length content) ++ "\n\n" ++
 																						content
