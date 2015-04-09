module Game.Game where

import Util exposing (..)

import Dict exposing (Dict)
import Signal

import Maybe as M exposing (andThen, oneOf)
import Debug 

type alias Point a =  (a, a)
type alias Coord = Point Int
type alias Vec = Coord

type Dir = UpDir | LeftDir | RightDir | DownDir
type Corner = TopLeft | TopRight | BotRight | BotLeft

type RotateDir = CW | CCW


type alias Rotate = {square : SquareId, dir : RotateDir}
type Event = RotateEvent Rotate

type alias Rotation = {dir: RotateDir, pivot:Corner, dest:Coord, progress:Int, final:Int}
type alias Square = {pos:Coord, rotation:Maybe Rotation}

type alias SquareId = Int

type alias Match = 
  { angle    : Int
  , dest     : Coord
  }

type alias Model = {squares:Dict SquareId Square, bounds: Coord}
              
              
initial : Model
initial = let squares = List.map2 (,) [1..10] coords 
              coords  = List.map makeSquare [ 
                (1, 1), (2, 1), (3, 1), (1, 2), 
                (1, 3), (1, 4), (3, 2), (3, 3), 
                (3, 4), (2, 4), (4, 6), (5, 5) 
              ]
        in
          {squares =  Dict.fromList squares,  bounds = (20, 20)}
          
          
square : Model -> SquareId -> Maybe Square
square model sq = Dict.get sq model.squares


occupied : Model -> Coord -> Bool
occupied model p = List.any (\s -> s.pos == p) (Dict.values model.squares)



rotation' : Coord -> Rotation -> (Coord, Int)
rotation' (x, y) r = 
  let pivot = case r.pivot of
        TopLeft -> (x, y)
        TopRight -> (x + 1,  y)
        BotRight -> (x + 1,  y + 1)
        BotLeft ->  (x,      y + 1)
      sign = case r.dir of 
        CW  -> 1
        CCW -> -1
  in (pivot, sign * r.progress)    
   
  

rotation : Square -> Maybe (Coord, Int)
rotation sq = Maybe.map (rotation' sq.pos) sq.rotation 


add : Coord -> Vec -> Coord
add (x, y) (x', y') = (x + x', y + y')

sub : Coord -> Vec -> Coord
sub (x, y) (x', y') = (x - x', y - y')

scale :  Vec -> Int -> Coord
scale (x, y) s = (x * s, y * s)


    
-- Try a particular rotation for a square around a corner
rotateCorner : Model -> Coord -> RotateDir -> Corner -> Maybe Match
rotateCorner model pos dir corner = 
    let rel v = relative pos corner v
        occ v = occupied model (rel v)
        maybeMatch = match dir occ
        
    in Maybe.map (\m -> {m | dest <- rel m.dest}) maybeMatch
    

    
runEvent : Event -> Model -> Model    
runEvent e model = case e of
  RotateEvent r -> applyRotate r model 
    
    
tryRotate : Model -> Rotate -> Maybe Rotation
tryRotate model r = 
  square model r.square `andThen` \sq ->
    let rc corner = Maybe.map (toRotation r.dir corner) 
          (rotateCorner model sq.pos r.dir corner)
    in oneOf (List.map rc allCorners)
    

applyRotate : Rotate -> Model -> Model    
applyRotate r model = case tryRotate model r of
  Just rot  -> setSquare r.square (\sq -> {sq | rotation <- Just rot}) model
  Nothing   -> Debug.crash "applyRotate: invalid rotation event"

toRotation : RotateDir -> Corner -> Match -> Rotation
toRotation dir corner m = 
  { dir = dir
  , pivot = corner
  , dest = m.dest
  , progress = 0
  , final = m.angle
  }


allCorners : List Corner
allCorners = [TopLeft, TopRight, BotLeft, BotRight]    
    
    
xorMaybe : Maybe a -> Maybe a -> Maybe a
xorMaybe ma mb = case (ma, mb) of
  (Nothing, Just b) -> Just b
  (Just a, Nothing) -> Just a
  (Just a, Just b)  -> Nothing   --ambigous
    
-- Try a rotation using the arrow key direction
rotateDir' : Model -> Coord -> Dir -> Maybe Rotation
rotateDir' model pos dir = 
  let rc d c = Maybe.map (toRotation d c) (rotateCorner model pos d c)    
  in case dir of 
        LeftDir   -> xorMaybe (rc CW TopLeft)     (rc CCW BotLeft) 
        RightDir  -> xorMaybe (rc CCW TopRight)   (rc CW BotRight)
        UpDir     -> xorMaybe (rc CW TopLeft)     (rc CCW TopRight)
        DownDir   -> xorMaybe (rc CCW BotLeft) (rc CW BotRight) 

rotateDir : Model -> SquareId -> Dir -> Maybe Event
rotateDir model id dir = 
  let toEvent r = RotateEvent {square = id, dir = r.dir}
  in square model id `andThen` \sq ->
      case sq.rotation of
        Nothing -> Maybe.map toEvent (rotateDir' model sq.pos dir)
        _       -> Nothing
        

relative  : Coord -> Corner -> Vec ->  Coord
relative pos corner (x, y)  = 
  let (r, u) = cornerBasis corner
  in pos `add` (r `scale` x) `add` (u `scale` y)  
      
      
cornerBasis : Corner -> (Vec, Vec)
cornerBasis c = case c of 
    TopLeft  -> ( (1,   0),  (0, -1)) 
    TopRight -> ( (0,   1),  (1,  0)) 
    BotRight -> ((-1,   0),  (0,  1)) 
    BotLeft  -> ( (0,  -1), (-1,  0))       
  


match : RotateDir -> (Vec -> Bool) -> Maybe Match
match dir = case dir of 
  CW -> matchCw
  CCW -> matchCcw
  
matchCw : (Vec -> Bool) -> Maybe Match
matchCw occ = 
  let up      = occ (0, 1)
      down    = occ (0, -1)
      left    = occ (-1, 0)
      clear2  = occ (-1, 1)
      
      match = up && (not down) && (not left)
       
  in if | match && clear2 -> Just {dest = (-1, 1), angle = 180}
        | match           -> Just {dest = (-1, 0), angle = 90}
        | otherwise -> Nothing
        
        
matchCcw : (Vec -> Bool) -> Maybe Match
matchCcw occ = 
  let left    = occ (-1, 0)
      right   = occ (1, 0)
      up      = occ (0, 1)
      clear2     = occ (-1, 1)
      
      match = left && (not right) && (not up)
       
  in if | match && clear2 -> Just {dest = (-1, 1), angle = 180}
        | match           -> Just {dest = (0,  1), angle = 90}
        | otherwise -> Nothing        
        


  
setSquare : SquareId -> (Square -> Square) -> Model -> Model
setSquare k f model = {model | squares <- Dict.update k (Maybe.map f) model.squares}



animateRotation : Int -> Square -> Rotation -> Square
animateRotation dt sq rot = 
  let update = {rot | progress <- min (rot.progress + dt * 10) rot.final}
  in  if (rot.progress < rot.final) 
        then {sq | rotation <- Just update}
        else {sq | pos <- rot.dest, rotation <- Nothing}
    
animateSquare : Int -> SquareId -> Square -> Square
animateSquare dt k sq = 
  case sq.rotation of 
    Just rotation -> animateRotation dt sq rotation
    Nothing       -> sq

animate : Int -> Model -> Model
animate dt model = {model | squares <- Dict.map (animateSquare dt) model.squares}
  

makeSquare : Coord -> Square
makeSquare pos = {pos = pos, rotation = Nothing}


      