{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TupleSections, TemplateHaskell, RankNTypes #-}

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
import Control.Lens
import Control.Monad

import Squares.Game.Types 


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


