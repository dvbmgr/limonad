

--  Copyright (C) 2013 David Baumgartner
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2, or (at your option)
--  any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
--  Boston, MA 02110-1301, USA.

module Darcs.OutCommands.ShowTags ( showTags ) where
import Darcs.Repository ( readRepo, withRepositoryDirectory, RepoJob(..) )
import Darcs.Patch.Info ( piTag )
import Darcs.Patch.Set ( tags )
import Darcs.Patch.Info


showTags :: String -> IO String
showTags repository =
  withRepositoryDirectory [] repository $ RepoJob $ \repository -> do
    patches <- readRepo repository
    return $ parseTags (tags patches)
  where
    parseTags :: [PatchInfo] -> String
    parseTags tags@(x:xs) = case piTag x of
        Just tag ->
          tag
        Nothing ->
          "There's no tag for this repository"