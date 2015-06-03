{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TupleSections, TemplateHaskell #-}

module Types where

import Data.Map (Map)

import Control.Monad

import Control.Lens
import Squares.Types
import Squares.Game


data Model = Model 
  { _model_game :: Game
  , _model_selected :: Maybe SquareId
  , _model_keymap :: Map Int GameInput
  , _model_connected :: Bool
  }  
  
data Action = Select (Maybe SquareId) 
            | GameAction { _game_event :: GameEvent } 
            | Animate { _animate_action :: Int } 
            | MadeConnection  
              deriving (Show)


liftM concat $ mapM makeLenses
  [ ''Model
  , ''Action
  ]
