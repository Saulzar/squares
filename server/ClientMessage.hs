
module ClientMessage (
    ClientMessage (ClientChat, Login, Hello)
  ) where


import Data.Aeson
import GHC.Generics

import Generics.Generic.Aeson

import Data.Text




data ClientMessage 
  = Hello 
  | Login {name :: !Text} 
  | ClientChat {message :: !Text} deriving (Show, Generic)
  
instance FromJSON ClientMessage where
  parseJSON = gparseJson
  
instance ToJSON ClientMessage where
  toJSON = gtoJson