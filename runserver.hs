import Darcs.Commands
import Darcs.OutCommands.ShowTags
import GHC.IO.Handle
import System.IO
import Server
import Templates
import System.Directory
import System.IO.Unsafe (unsafeDupablePerformIO)
import Control.Monad hiding (join)
import qualified Data.Conduit.List

repos_path :: String
repos_path = "./repos"

repositoryDescrition :: String -> IO String 
repositoryDescrition name = do
	showTags (repos_path ++ "/" ++ name)

getDirectoryList :: ViewParam -> View
getDirectoryList params = basicViewIO (getDirectoryContents repos_path >>= \repos -> renderFile [] [("repositories", (reps repos))] "templates/repos_index.html")
	where
		reps :: [FilePath] -> [[(String,String)]]
		reps = map (\a -> [("name", a),("slug", slugifyString a),("desc", readFromUnicode $ unsafeDupablePerformIO $ repositoryDescrition a)]) . filter (\a -> ((a !! 0) /= '.'))

getRepositoryTags :: ViewParam -> View
getRepositoryTags params = basicViewIO (repositoryDescrition (readGet "n" params))

getCSS :: ViewParam -> View
getCSS _ = ViewIO (HttpReturnCode 200) "text/css" Nothing [] (readFile "static/main.css")

redirectCode :: ViewParam -> View
redirectCode _ = redirectPermanently "http://github.com/davbaumgartner/DarcsUI"

getNewRepo :: ViewParam -> View
getNewRepo _ = renderFileToView [] [] "new_repo.html" 

main :: IO ()
main = runServer 8080 ([Route "*" "/" getDirectoryList,
						Route "GET" "/main.css" getCSS,
						Route "GET" "/code" redirectCode,
						Route "GET" "/tags" getRepositoryTags])