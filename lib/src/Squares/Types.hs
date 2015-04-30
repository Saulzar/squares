{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TupleSections, OverloadedLists, TemplateHaskell, RankNTypes #-}
module Squares.Types 
  ( Rotation (..)
  , Square(..)
  , SquareId (..)
  , Game(..)
  , GameEvent(..)
  , GameInput(..)
  
  , Coord, Vec
  , V2 (..)
  
  
  , Dir(..)
  , Corner(..)
  , RotateDir(..)
  
  , rot_dir, rot_pivot, rot_dest, rot_angle
  , square_rotation, square_position
  , game_squares, game_bounds
    
  )
  where

import qualified Data.Map as Map
import Data.Map (Map)

import Control.Lens
import Linear hiding (angle, basis)


type Coord = V2 Int
type Vec = V2 Int

data Dir = UpDir | LeftDir | RightDir | DownDir  deriving (Show, Eq, Ord)
data Corner = TopLeft | TopRight | BotRight | BotLeft deriving (Show, Eq, Ord)

data RotateDir = CW | CCW deriving (Show, Eq, Ord)

data GameInput = ArrowKey Dir deriving (Show, Eq)

data Rotation = Rotation 
  {  _rot_dir     :: !RotateDir
  , _rot_pivot    :: !Corner
  , _rot_dest     :: !Coord
  , _rot_angle    :: !Int
  } deriving (Show)
  
data Square = Square
  {  _square_rotation :: Maybe (Rotation, Int)
  ,  _square_position :: Coord
  } deriving (Show)

  
newtype SquareId = SquareId { unSquare :: Int } deriving (Eq, Ord, Show)

data Game = Game
  { _game_squares  :: Map SquareId Square
  , _game_bounds   :: !Coord
  } deriving (Show)
  
  
data GameEvent = RotateEvent SquareId RotateDir deriving Show
  
$( makeLenses ''Game)
$( makeLenses ''Square)
$( makeLenses ''Rotation)