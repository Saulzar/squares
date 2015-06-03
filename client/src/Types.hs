{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TupleSections, TemplateHaskell #-}

module Types where

import Data.Map (Map)

import Control.Monad

import Control.Lens
import Squares.Types

import Data.Text (Text)


data Model = Login | Playing { _model_playing :: PlayingState } 


data PlayingState = PlayingState 
  { _model_game :: Game
  , _model_user :: UserId
  , _model_selected :: Maybe SquareId
  , _model_keymap :: Map Int GameInput
  }  


  
  
  
data Action = Select (Maybe SquareId) 
            | MoveAction { _move_action    :: UserMove }
            | ChatAction { _chat_action    :: Text }
            | Animate    { _animate_action :: Int } 
            | MadeConnection  
              deriving (Show)


liftM concat $ mapM makeLenses
  [ ''Model
  , ''Action
  ]
