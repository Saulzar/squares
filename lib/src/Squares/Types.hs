{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TupleSections, TemplateHaskell, RankNTypes, OverloadedStrings #-}

module Squares.Types 
  ( module Squares.Game.Types
  , ServerMsg(..)
  , ClientMsg(..)
  , loginOk
  , loginError
  
  , LoginResponse
  , LoginError(..)
  , Login(..)
  
  , _ServerEvent, _ServerFrame, _ServerReset, _ServerError
  , _ClientChat, _ClientMove, _ClientFrame
  
  ) where




import GHC.Generics
import qualified Data.Map.Strict as M

import Data.Text (Text)
import Data.Text.Binary

import Data.Binary
import Control.Lens hiding ( (.=) )
import Control.Monad
import Data.Functor

import Squares.Game.Types 
import Data.Aeson

data ServerMsg 
    = ServerEvent UserEvent
    | ServerFrame [UserMove]
    | ServerReset !Game
    | ServerError !Text
      deriving (Show, Generic)
      
data ClientMsg 
    = ClientChat  !Text
    | ClientMove  !GameMove 
    | ClientFrame 
      deriving (Show, Generic)

      
data Login = Login UserName deriving (Show, Generic)
type LoginResponse = Either LoginError (UserId, Game) 

loginOk :: (UserId, Game) -> LoginResponse
loginOk = Right

loginError :: LoginError -> LoginResponse
loginError = Left

data LoginError = LoginFull | LoginDataError !Text deriving (Show, Generic)

      
liftM concat $ mapM makePrisms 
    [ ''ServerMsg
    , ''ClientMsg
    ]

instance Binary ServerMsg
instance Binary ClientMsg
instance Binary LoginError  
instance Binary Login


-- One constructor/argument deriving seems to go wrong

instance ToJSON Login where
    toJSON (Login user) = object ["user" .= user]
     
instance FromJSON Login where
     parseJSON (Object v) = Login <$> v .: "user"
     parseJSON _          = mzero     

instance ToJSON ServerMsg
instance ToJSON ClientMsg
instance ToJSON LoginError  
-- instance ToJSON Login

instance FromJSON ServerMsg
instance FromJSON ClientMsg
instance FromJSON LoginError  
-- instance FromJSON Login
