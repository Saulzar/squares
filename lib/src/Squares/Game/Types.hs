{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TupleSections, TemplateHaskell, RankNTypes #-}
module Squares.Game.Types 
  ( Rotation (..)
  , Square(..)
  , SquareId (..)
  , Game(..)
  , GameMove(..)
  , GameEvent(..)
  , GameInput(..)
  
  , UserId(..)
  , UserMove
  , UserEvent
  
  , User(..)
  
  , Coord, Vec
  , V2 (..)
  
  
  , Dir(..)
  , Corner(..)
  , RotateDir(..)
  
  , rot_dir, rot_pivot, rot_dest, rot_angle
  , square_rotation, square_position
  , game_squares, game_bounds, game_num_players, game_players, game_started
    
  )
  where

import qualified Data.Map as Map
import Data.Map (Map)

import Data.Text.Binary
import Data.Text (Text)

import Control.Monad
import Control.Lens
import Linear hiding (angle, basis)
import GHC.Generics
import Data.Binary


type Coord = V2 Int
type Vec = V2 Int

data Dir = UpDir | LeftDir | RightDir | DownDir  deriving (Show, Eq, Ord, Generic)
data Corner = TopLeft | TopRight | BotRight | BotLeft deriving (Show, Eq, Ord, Generic)

data RotateDir = CW | CCW deriving (Show, Eq, Ord, Generic)

data GameInput = ArrowKey Dir deriving (Show, Eq, Generic)

data Rotation = Rotation 
  {  _rot_dir     :: !RotateDir
  , _rot_pivot    :: !Corner
  , _rot_dest     :: !Coord
  , _rot_angle    :: !Int
  } deriving (Show, Generic)
  
data Square = Square
  {  _square_rotation :: Maybe (Rotation, Int)
  ,  _square_position :: Coord
  } deriving (Show, Generic)

  
newtype SquareId = SquareId { unSquare :: Int } deriving (Eq, Ord, Show, Generic, Binary)

newtype UserId = UserId { unUser :: Int } deriving (Eq, Ord, Show, Generic)
data User = User {  _user_name :: !Text } deriving (Show, Generic)


data Game = Game
  { _game_squares  :: Map SquareId Square
  , _game_players    :: Map UserId User
  , _game_bounds   :: !Coord
  , _game_num_players :: !Int
  , _game_started     :: !Bool
  } deriving (Show, Generic)
  
  
data GameMove = RotateMove SquareId RotateDir deriving (Show, Generic)



data GameEvent
    = ChatEvent !Text
    | JoinEvent !User
    | UserLeave 
    | UserDisconnect
    | UserReconnect
        deriving (Show, Generic)

      
type UserMove = (UserId, GameMove)
type UserEvent = (UserId, GameEvent)
  
instance Binary Game
instance Binary Square
instance Binary Rotation
instance Binary Dir
instance Binary Corner
instance Binary RotateDir
instance Binary GameMove
instance Binary GameEvent
instance Binary User
instance Binary UserId



instance Binary a => Binary (V2 a) where
  put = putLinear
  get = getLinear
  
  
liftM concat $ mapM makeLenses
  [ ''Game
  , ''Square
  , ''Rotation
  ]  

  
  