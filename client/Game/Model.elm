module Game.Model where

import Util exposing (..)

import Dict exposing (Dict)
import Signal


type alias Point a = {x:a, y:a}
type alias Coord = Point Int
type alias Vec = Coord

type Dir = UpDir | LeftDir | RightDir | DownDir

type Event = Rotate Dir SquareId


type alias Rotation = {rotation:Int, final:Int, clockwise: Bool, pivot:Int, dest:Coord}
type alias Square = {pos:Coord, rotation:Maybe Rotation}

type alias SquareId = Int
type alias Model = {squares:Dict SquareId Square, bounds: Coord}
              
              
event : Event -> Model -> Model
event event = case event of
  Rotate dir k -> rotateEvent dir k  


initial : Model
initial = let squares = List.map2 (,) [1..10] coords 
              coords  = List.map makeSquare [ 
                (1, 1), (2, 1), (3, 1), (1, 2), 
                (1, 3), (1, 4), (3, 2), (3, 3), 
                (3, 4), (2, 4), (4, 6), (5, 5) 
              ]
        in
          {squares =  Dict.fromList squares,  bounds = {x = 20, y = 20}}


occupied : Model -> Coord -> Bool
occupied model p = List.any (\s -> s.pos == p) (Dict.values model.squares)



rotation' : Coord -> Rotation -> (Coord, Int)
rotation' pos r = 
  let pivot = case r.pivot of
        0 -> pos
        1 -> {x = pos.x + 1, y = pos.y}
        2 -> {x = pos.x + 1, y = pos.y + 1}
        3 -> {x = pos.x, y = pos.y + 1}
      sign = if r.clockwise then 1 else -1
  in (pivot, sign * r.rotation)    
   
  

rotation : Square -> Maybe (Coord, Int)
rotation sq = Maybe.map (rotation' sq.pos) sq.rotation 


add : Coord -> Vec -> Coord
add a b = {x = a.x + b.x, y = a.y + b.y}

sub : Coord -> Vec -> Coord
sub a b = {x = a.x - b.x, y = a.y - b.y}

scale :  Vec -> Int -> Coord
scale a s = {x = s * a.x, y = s * a.y}




relRotate : Coord -> Model -> Int -> Vec -> Vec -> Maybe Rotation
relRotate pos model corner r u = 
  let occ x y = occupied model (rel x y) 
      rel x y = pos `add` (r `scale` x) `add` (u `scale` y)
      
      top   = occ 0 1 
      right = occ 1 0
      left  = occ -1 0

      pivot = if left then corner else (corner + 1) % 4
      dir = if left then -1 else 1
 
      far = occ dir 1
      dest = if (not far) then rel dir 1 else rel 0 1
      
      rotation = {rotation = 0, final = if far then 90 else 180, 
           clockwise = right, pivot = pivot, dest = dest}
      
  in bool (left `xor` right && (not top))  rotation



tryRotate : Coord -> Model -> Dir -> Maybe Rotation
tryRotate pos model dir = let rotate = relRotate pos model in 
  case dir of 
    UpDir    -> rotate 0 {x = 1,  y = 0}  {x = 0, y = -1} 
    RightDir -> rotate 1 {x = 0,  y = 1} {x = 1, y = 0} 
    DownDir  -> rotate 2 {x = -1, y = 0}  {x = 0, y = 1} 
    LeftDir  -> rotate 3 {x = 0,  y = -1}  {x = -1, y = 0} 




tryRotation : Model -> Dir -> Square -> Square
tryRotation model dir square = {square | rotation <- concatMaybes square.rotation (tryRotate square.pos model dir)}
  
  
setSquare : SquareId -> (Square -> Square) -> Model -> Model
setSquare k f model = {model | squares <- Dict.update k (Maybe.map f) model.squares}


rotateEvent : Dir -> SquareId -> (Model -> Model)
rotateEvent dir k model = setSquare k (tryRotation model dir) model


animateRotation : Int -> Square -> Rotation -> Square
animateRotation dt sq rot = 
  let update = {rot | rotation <- min (rot.rotation + dt * 10) rot.final}
  in  if (rot.rotation < rot.final) 
        then {sq | rotation <- Just update}
        else {sq | pos <- rot.dest, rotation <- Nothing}
    
animateSquare : Int -> SquareId -> Square -> Square
animateSquare dt k sq = 
  case sq.rotation of 
    Just rotation -> animateRotation dt sq rotation
    Nothing       -> sq

animate : Int -> Model -> Model
animate dt model = {model | squares <- Dict.map (animateSquare dt) model.squares}
  


makeSquare : (Int, Int) -> Square
makeSquare (x, y) = {pos = {x = x, y = y}, rotation = Nothing}


      