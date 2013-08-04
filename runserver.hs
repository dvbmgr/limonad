import Server
import Templates
import System.Directory
import Control.Monad hiding (join)
import qualified Data.Conduit.List

repos_path :: String
repos_path = "./repos"

getDirectoryList :: ViewParam -> View
getDirectoryList params = basicViewIO (getDirectoryContents repos_path >>= \repos -> renderFile [] [("repositories", (reps repos))] "templates/repos_index.html")
	where
		reps :: [FilePath] -> [[(String,String)]]
		reps = map (\a -> [("name", a),("slug", slugifyString a)]) . filter (\a -> ((a !! 0) /= '.'))

getCSS :: ViewParam -> View
getCSS _ = ViewIO (HttpReturnCode 200) "text/css" Nothing [] (readFile "static/main.css")

redirectCode :: ViewParam -> View
redirectCode _ = redirectPermanently "http://github.com/davbaumgartner/DarcsUI"

getNewRepo :: ViewParam -> View
getNewRepo _ = renderFileToView [] [] "new_repo.html" 

main :: IO ()
main = runServer 8080 ([Route "*" "/" getDirectoryList,
						Route "GET" "/main.css" getCSS,
						Route "GET" "/code" redirectCode])