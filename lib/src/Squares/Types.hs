module Squares.Types
  ( ServerMessage (..)
  , UserId (..)
  , User(..)
  , ClientMessage (..)
  
  , user_id
  , user_name
  
  ) where


import GHC.Generics
import qualified Data.Map.Strict as M

import Data.Text
import Data.Text.Binary

import Data.Binary
import Control.Lens

import Squares.Game.Types (Game)


newtype UserId = UserId { unUser :: Int } deriving (Eq, Ord, Show, Generic)
data User = User { _user_id :: !UserId, _user_name :: !Text } deriving (Show, Generic)


$( makeLenses ''User)


data ServerMessage 
    = Connected !UserId !Text
    | Disconnected !UserId
    | Chat !UserId !Text
    | ServerError !Text 
    | Welcome !Game [User] !UserId
      deriving (Show, Generic)
      
data ClientMessage
   = ClientChat !Text
   | ClientSettings !Text
      deriving (Show, Generic)

      
  
instance Binary ServerMessage 
instance Binary ClientMessage
instance Binary User
instance Binary UserId