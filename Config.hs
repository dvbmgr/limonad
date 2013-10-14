-------------------------
-- Configuration       --
-------------------------

module Config (repos_path, port) where
import Network.Socket.Internal
-- Repository path
repos_path :: String
repos_path = "./repos"

-- Port
port :: PortNumber
port = 3000