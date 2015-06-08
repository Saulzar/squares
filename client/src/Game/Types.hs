{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TupleSections, TemplateHaskell #-}

module Game.Types where

import Data.Map (Map)

import Control.Monad

import Control.Lens
import Squares.Types

import Data.Text (Text)



type KeyMap = Map Int GameInput

data Settings = Settings 
  { _settings_keyMap :: KeyMap
  }

data GameModel = GameModel 
  { _gm_game :: Game
  , _gm_user :: UserId
  , _gm_selected :: Maybe SquareId
  }  
  
  
data GameAction 
    = Select (Maybe SquareId) 
    | MoveAction UserMove
    | ChatAction Text
    | Animate    Int  
      deriving (Show)


liftM concat $ do
  lenses <- mapM makeLenses
    [ ''Settings, ''GameModel]

  prisms <- mapM makePrisms [ ''GameAction ]
  return (lenses ++ prisms)


