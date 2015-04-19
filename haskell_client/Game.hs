{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TupleSections, OverloadedLists, TemplateHaskell #-}
module Game 
  ( Rotation (..)
  , Square(..)
  , SquareId (..)
  , Game(..)
  , Coord
  , Vec
  
  , rot_dir, rot_pivot, rot_dest, rot_progress, rot_final
  , square_rotation, square_position
  , game_squares, game_bounds
  
  , initialGame
  
  )
  where

import qualified Data.Map as Map
import Data.Map (Map)

import Control.Lens

import Data.Monoid

import Control.Monad
import Control.Monad.IO.Class



type Coord = (Int, Int)
type Vec = (Int, Int)

data Dir = UpDir | LeftDir | RightDir | DownDir
data Corner = TopLeft | TopRight | BotRight | BotLeft

data RotateDir = CW | CCW



data Rotation = Rotation 
  {  _rot_dir     :: !RotateDir
  , _rot_pivot    :: !Corner
  , _rot_dest     :: !Coord
  , _rot_progress :: !Int
  , _rot_final    :: !Int
  }
  
data Square = Square
  {  _square_rotation :: Maybe Rotation
  ,  _square_position :: Coord
  }

  
newtype SquareId = SquareId { unSquare :: Int } deriving (Eq, Ord, Show)

data Game = Game
  { _game_squares  :: Map SquareId Square
  , _game_bounds   :: !Coord
  }
  
  
$( makeLenses ''Game)
$( makeLenses ''Square)
$( makeLenses ''Rotation)


square :: Coord -> Square
square pos = Square 
  { _square_rotation = Nothing
  , _square_position = pos }

initialGame :: Game
initialGame = Game 
  { _game_squares = Map.fromList $ zip ids squares 
  , _game_bounds  = (20, 10) }
  where
    ids = map SquareId [1..]
    squares = map square [(3, 3), (3, 4), (4, 3), (4, 4), (4, 5), (5, 5), (6, 4)] 

