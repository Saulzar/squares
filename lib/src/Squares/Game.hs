{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TupleSections, OverloadedLists, TemplateHaskell, RankNTypes #-}
module Squares.Game 
  ( module Squares.Types
  , initialGame
  , runEvent
  , rotateDir
  , animate
  , progress
  )
  where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe

import Control.Lens
import Control.Monad
import Linear hiding (angle, basis, rotate)

import Debug.Trace

import Squares.Types


mkSquare :: Coord -> Square
mkSquare pos = Square 
  { _square_rotation = Nothing
  , _square_position = pos }

initialGame :: Game
initialGame = Game 
  { _game_squares = Map.fromList $ zip ids squares 
  , _game_bounds  = V2 20 10 }
  where
    ids = map SquareId [1..]
    squares = map mkSquare [V2 3 3, V2 3 4, V2 4 3, V2 4 4, V2 4 5, V2 5 5, V2 6 4] 
    
    
    

occupied :: Game -> Coord -> Bool
occupied model p = any ((== p) . (_square_position)) $ model^.game_squares 


square :: SquareId -> Traversal' Game Square
square i = game_squares . at i . traverse

cornerPos :: Corner -> Coord -> Coord
cornerPos TopLeft  (V2 x y) = V2  x       y
cornerPos TopRight (V2 x y) = V2 (x + 1)  y
cornerPos BotRight (V2 x y) = V2 (x + 1) (y + 1)
cornerPos BotLeft  (V2 x y) = V2  x      (y + 1)
        

rotationAngle :: (Rotation, Int) -> Int
rotationAngle (r, progress) = case r^.rot_dir of 
        CW  ->  progress
        CCW -> -progress

  
progress :: SquareId -> Game -> Maybe (Coord, Int)
progress i game = do
  sq      <- game ^? square i
  (r, p)  <- sq^.square_rotation 
  return (cornerPos (r^.rot_pivot) (sq^.square_position), rotationAngle (r, p)) 
  

   
type Match = (Coord, Int)


match :: RotateDir -> (Vec -> Bool) -> Maybe Match
match CW  = matchCw
match CCW = matchCcw
  
matchCw :: (Vec -> Bool) -> Maybe Match
matchCw occ | match && clear2 = Just (V2 (-1) 1, 180)
            | match           = Just (V2 (-1) 0, 90)
            | otherwise = Nothing 
  where 
      up      = occ $ V2 0 1
      down    = occ $ V2 0 (-1)
      left    = occ $ V2 (-1) 0
      clear2  = not $ occ $ V2 (-1) 1
      
      match = up && (not down) && (not left)
      
      
matchCcw :: (Vec -> Bool) -> Maybe Match
matchCcw occ | match && clear2 = Just (V2 (-1) 1, 180)
             | match           = Just (V2   0  1, 90)
             | otherwise = Nothing 
  where
      left    = occ $ V2 (-1) 0
      right   = occ $ V2 1 0
      up      = occ $ V2 0 1
      clear2     = not $ occ $ V2 (-1) 1
      
      match = left && (not right) && (not up)


    
-- Try a particular rotation for a square around a corner
tryCorner :: Game -> Coord -> RotateDir -> Corner -> Maybe Rotation
tryCorner model pos dir corner = fmap (toRot . over _1 rel) $ match dir occ where
    rel v = relative pos corner v
    occ v = occupied model (rel v)
    toRot = toRotation dir corner

allCorners :: [Corner]
allCorners = [TopLeft, TopRight, BotLeft, BotRight]        
    
tryRotate :: Game -> SquareId -> RotateDir -> Maybe Rotation
tryRotate model i dir = do
  sq <- model^?square i
  guard (isNothing $ _square_rotation sq)  
  msum $ map (tryCorner' sq) allCorners
  
  where
    tryCorner' :: Square -> Corner -> Maybe Rotation
    tryCorner' sq = tryCorner model (sq^.square_position) dir
    
    
toRotation :: RotateDir -> Corner -> Match -> Rotation
toRotation dir corner (dest, angle) = Rotation
  { _rot_dir = dir
  , _rot_pivot = corner
  , _rot_dest = dest
  , _rot_angle = angle
  }


xorMaybes :: Maybe a -> Maybe a -> Maybe a
xorMaybes ma mb = case (ma, mb) of
  (Nothing, Just b) -> Just b
  (Just a, Nothing) -> Just a
  _   -> Nothing   --ambigous
    
    
-- Try a rotation using the arrow key direction
rotateDir :: Game -> SquareId -> Dir -> Maybe GameEvent
rotateDir model i dir =  do
  sq <- model^?square i 
  
  traceShow (i, dir, sq) $ return ()
  
  
  guard (isNothing $ sq ^. square_rotation)
  
  fmap (RotateEvent i . _rot_dir) $ 
    rotateDir' model (sq^.square_position) dir  
  
unique :: [a] -> Maybe a
unique [a] = Just a
unique _   = Nothing
  

rotateDir' :: Game -> Coord -> Dir -> Maybe Rotation
rotateDir' model pos dir = traceShow (pos, catMaybes possible) $ unique search
  where
    rotate rotDir corner = tryCorner model pos rotDir corner
    dirs = [CW, CCW]
    possible = [rotate d c | d <- dirs, c <- allCorners]
    search = filter isDir . catMaybes $ possible
 
    isDir r = (r^.rot_dest - pos) `dot` dirVec dir > 0 

relative  :: Coord -> Corner -> Vec ->  Coord
relative pos corner v  = {-traceShow (basis, corner) $ -} (basis !* v) + pos where
  basis = cornerBasis corner 
 
      
      
dirVec :: Dir -> Vec
dirVec LeftDir  = V2 (-1) 0
dirVec RightDir = V2 1 0
dirVec UpDir    = V2 0 (-1)
dirVec DownDir  = V2 0 1

      
cornerBasis :: Corner -> V2 Vec
cornerBasis c = case c of 
    TopLeft  -> V2 (V2   1    0) ( V2   0  (-1)) 
    TopRight -> V2 (V2   0    1)  (V2   1    0)
    BotRight -> V2 (V2 (-1)   0)  (V2   0    1)
    BotLeft  -> V2 (V2   0  (-1)) (V2 (-1)   0)       
  
  

rotateEvent :: SquareId -> RotateDir -> Game -> Game
rotateEvent i dir game = traceShow rotation $ maybe game startRotate rotation where
  rotation      = tryRotate game i dir 
  startRotate r = game & (square i . square_rotation) .~ Just (r, 0)
  
  
runEvent :: GameEvent -> Game -> Game
runEvent (RotateEvent i dir) = rotateEvent i dir 


advance :: Int -> Square -> Square
advance dt sq = case sq^.square_rotation of
    Just (r, angle) -> if (angle + dt < r ^. rot_angle)  
      then sq & (square_rotation . _Just . _2) .~ (angle + dt)
      else sq & square_rotation .~ Nothing
              & square_position .~ (r ^. rot_dest)
    _ -> sq
      

animate :: Int -> Game -> Game
animate dt = over (game_squares . traverse) (advance dt)
  