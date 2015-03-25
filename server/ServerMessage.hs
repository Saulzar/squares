module ServerMessage (

    ServerMessage (Connected, Disconnected, Chat)
  )
  
  
where

import Data.Aeson
import GHC.Generics

import Data.Text



data ServerMessage 
    = Connected { id :: !Int, name :: !Text}
    | Disconnected {id :: !Int }
    | Chat { sender :: !Int, message :: !Text}
    | Error { reason :: !Text } 
      deriving (Show, Generic)
  
instance FromJSON ServerMessage
instance ToJSON ServerMessage