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
 
module DarcsAPI (	showTags,
					showAuthor,
					showAge,
					showCommitNth,
					getChanges,
					mkTarBall ) where
	import Darcs.Repository ( readRepo, withRepositoryDirectory, RepoJob(..) )
	import Darcs.Patch.Set
	import Darcs.Patch.Info
	import Darcs.Witnesses.Ordered
	import System.Time
	import System.Locale
	import System.Directory
	import Darcs.Patch.PatchInfoAnd
	import Data.String.Utils (split, join)
	import Data.List
	import qualified Codec.Archive.Tar as Tar

	rmNothing :: [Maybe String] -> [String] -> [String]
	rmNothing [] xs = xs
	rmNothing xp@(x:xs) out = case x of
		Just f ->
			rmNothing xs (f:out)
		Nothing ->
			rmNothing xs out

	showTags :: String -> IO String
	showTags repository =
		withRepositoryDirectory [] repository $ RepoJob $ \repository -> do
			patches <- readRepo repository
			return $ join ", " (nub $ rmNothing (mapRL (piTag . info) (newset2RL patches)) [])

	formatTime ftime
			| btime <= 0 = "just now"
			| btime <= 1 = "a second"
			| btime <= 59 = (show $ round btime) ++ " seconds"
			| btime <= 119 = "a minute"
			| btime <= 3540 = (show $ round (btime/60)) ++ " minutes"
			| btime <= 7100 = "an hour"
			| btime <= 82800 = (show $ round ((btime+99)/3600)) ++ " hours"
			| btime <= 172000 = "a day"
			| btime <= 518400 = (show $ round ((btime+800)/(60*60*24))) ++ " days"
			| btime <= 1036800 = "a week"
			| otherwise = (show $ round ((btime+180000)/(60*60*24*7))) ++ " weeks"
		where 
			btime = read (head (split " " ftime)) :: Float

	showAge :: String -> IO String
	showAge repository = 
		withRepositoryDirectory [] repository $ RepoJob $ \repository -> do
			patches <- readRepo repository
			getClockTime >>= \clock -> return $ head (mapRL (parseTags clock . info) (newset2RL patches))
		where
			parseTags :: ClockTime -> PatchInfo -> String
			parseTags clock x = formatTime $ timeDiffToString $ (diffClockTimes clock (toClockTime $ piDate x)) 

	showCommitNth :: String -> IO Int 
	showCommitNth repository = 
		withRepositoryDirectory [] repository $ RepoJob $ \repository -> do
			readRepo repository >>= (return . lengthRL . newset2RL)

	showAuthor :: String -> IO String
	showAuthor repository =
		withRepositoryDirectory [] repository $ RepoJob $ \repository -> do
			patches <- readRepo repository
			return $ join ", " (nub (mapRL (piAuthor . info) (newset2RL patches)))


	getChanges :: String -> IO [(String, String, String, String, String, String, String, String)]
	getChanges repository = 
		withRepositoryDirectory [] repository $ RepoJob $ \repository -> do
			patches <- readRepo repository
			(getClockTime >>= \clock -> return $ addCommitId 1 (mapRL (mkInfo clock . info) (newset2RL patches)) [])
		where
			mkInfo :: ClockTime -> PatchInfo -> (String, String, String, String, String, String, String)
			mkInfo clock infos = (makeAltFilename infos,
								piName infos, 
								piAuthor infos, 
								(formatCalendarTime defaultTimeLocale "%c" $ piDate infos) ++ " (" ++ (formatTime $ timeDiffToString $ (diffClockTimes clock (toClockTime $ piDate infos))) ++ " ago)",
								join ", " $ piLog infos,
								"",
								""
								)

	addCommitId :: Int -> [(String, String, String, String, String, String, String)] -> [(String, String, String, String, String, String, String, String)] -> [(String, String, String, String, String, String, String, String)]
	addCommitId _ [] out = out
	addCommitId x ((a1, a2, a3, a4, a5, a6, a7):es) out = addCommitId (x+1) es ((show x, a1, a2, a3, a4, a5, a6, a7):out)

	{- TODO -}
	mkTarBall :: String -> String -> IO String 
	mkTarBall repo_path path = (doesFileExist tpath) >>= \exists ->
						if exists then do
							directory_contents <- getDirectoryContents (repo_path ++ "/" ++ path)
							Tar.create tpath (repo_path ++ "/" ++ path) $ filter (\a -> ((a !! 0) /= '.')) directory_contents
							return tpath
						else 
							return tpath
			where
				tpath = path ++ ".tar"
	{- /TODO -}