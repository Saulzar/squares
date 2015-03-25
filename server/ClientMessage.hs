
module ClientMessage (
    Login (Login), 
    ClientMessage (ClientChat)
  ) where


import Data.Aeson
import GHC.Generics

import Data.Text

data Login = 
  Login { name :: !Text } deriving (Show, Generic)


instance FromJSON Login
instance ToJSON Login


data ClientMessage =
  ClientChat { message :: !Text} deriving (Show, Generic)
  
instance FromJSON ClientMessage
instance ToJSON ClientMessage