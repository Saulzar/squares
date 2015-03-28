module ServerMessage (

    ServerMessage (Connected, Disconnected, Chat, ServerError, Welcome),
    User(User)
  )
  
  
where

import Data.Aeson
import Generics.Generic.Aeson
import GHC.Generics

import qualified Data.Map.Strict as M

import Data.Text

data User = User { userId :: !Int, userName :: !Text } deriving (Show, Generic)


data ServerMessage 
    = Connected {id :: !Int, name :: !Text}
    | Disconnected {id :: !Int}
    | Chat {sender :: !Int, message :: !Text}
    | ServerError {reason :: !Text} 
    | Welcome {id :: !Int, users :: [User]}
      deriving (Show, Generic)
  
instance FromJSON ServerMessage where
  parseJSON = gparseJson
  
instance ToJSON ServerMessage where
  toJSON = gtoJson
  
  
instance FromJSON User where
  parseJSON = gparseJson
  
instance ToJSON User where
  toJSON = gtoJson  