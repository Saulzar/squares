module Game.Game where

import Util exposing (..)

import Dict exposing (Dict)
import Signal


type alias Point a =  (a, a)
type alias Coord = Point Int
type alias Vec = Coord

type Dir = UpDir | LeftDir | RightDir | DownDir
type Corner = TopLeft | TopRight | BotRight | BotLeft

type Input = Arrows Dir SquareId

type Rotate = {square : SquareId, clockwise : Bool}

type alias Rotation = {clockwise: Bool, pivot:Corner, dest:Coord, progress:Int, final:Int}
type alias Square = {pos:Coord, rotation:Maybe Rotation}

type alias SquareId = Int



type alias Model = {squares:Dict SquareId Square, bounds: Coord}
              
              
initial : Model
initial = let squares = List.map2 (,) [1..10] coords 
              coords  = List.map makeSquare [ 
                (1, 1), (2, 1), (3, 1), (1, 2), 
                (1, 3), (1, 4), (3, 2), (3, 3), 
                (3, 4), (2, 4), (4, 6), (5, 5) 
              ]
        in
          {squares =  Dict.fromList squares,  bounds = {x = 20, y = 20}}
          
          
square : Model -> SquareId -> Maybe Square
square model sq = Dict.get sq model.squares


occupied : Model -> Coord -> Bool
occupied model p = List.any (\s -> s.pos == p) (Dict.values model.squares)



rotation' : Coord -> Rotation -> (Coord, Int)
rotation' (x, y) r = 
  let pivot = case r.pivot of
        TopLeft -> pos
        TopRight -> (x + 1,  y)
        BotRight -> (x + 1,  y + 1)
        BotLeft ->  (x,      y + 1)
      sign = if r.clockwise then 1 else -1
  in (pivot, sign * r.rotation)    
   
  

rotation : Square -> Maybe (Coord, Int)
rotation sq = Maybe.map (rotation' sq.pos) sq.rotation 


add : Coord -> Vec -> Coord
add (x, y) (x', y') = (x + x', y + y')

sub : Coord -> Vec -> Coord
sub (x, y) (x', y') = (x - x', y - y')

scale :  Vec -> Int -> Coord
sub (x, y) s = (x * s, y * s)


    
-- Try a particular rotation for a square around a corner
rotateCorner : Model -> Coord -> Bool -> Corner -> Maybe Rotation
rotateCorner model pos clockwise corner = 
    let rel v = relative pos corner v
        occ v = occupied model v
        
        match = if clockwise then matchCw else matchCcw
        
        toRotation m = 
          { clockwise = clockwise
          , pivot = corner
          , dest = rel m.dest
          , progress = 0
          , final = m.angle
          }
          
    in Maybe.map toRotation (match occ)
    
    
tryRotate : Model -> Rotate -> Maybe Rotation
tryRotate model {square, clockwise} = 
  square model sq `andThen` \sq ->
    let rc = rotateCorner model sq.pos clockwise
    in oneOf (List.map rc allCorners)

allCorners : List Corner
allCorners = [TopLeft, TopRight, BottomLeft, BottomRight]    
    
    
xorMaybe : Maybe a -> Maybe a -> Maybe a
xorMaybe ma mb = case (ma, mb) of
  (Nothing, Just b) -> b
  (Just a, Nothing) -> a
  (Just a, Just b)  -> Nothing   --ambigous
    
-- Try a rotation using the arrow key direction
rotateDir : Model -> Coord -> Dir -> Maybe Rotation
rotateDir model pos dir = 
  let rc = rotateCorner model pos    
  in case Dir of 
        LeftDir   -> xorMaybe (rc True TopLeft)     (rc False BottomLeft) 
        RightDir  -> xorMaybe (rc False TopRight)   (rc True BottomRight)
        UpDir     -> xorMaybe (rc True TopLeft)     (rc False TopRight)
        DownDir   -> xorMaybe (rc False BottomLeft) (rc True BottomRight) 
  
        

relative  : Coord -> Corner -> Vec ->  Coord
relative pos corner (x, y)  = 
  let (r, u) = cornerBasis corner
      in pos `add` (r `scale` x) `add` (u `scale` y))  
      
      
cornerBasis : Corner -> (Vec, Vec)
cornerBasis c = case c of 
    TopLeft  -> ( (1,   0),  (0, -1)) 
    TopRight -> ( (0,   1),  (1,  0)) 
    BotRight -> ((-1,   0),  (0,  1)) 
    BotLeft  -> ( (0,  -1), (-1,  0))       
  

type alias Match = 
  { distance : Int
  , dest     : Coord
  }
  
matchCw : (Vec -> Bool) -> Maybe Match
matchCw occ = 
  let up      = occ (0, 1)
      down    = occ (0, -1)
      left    = occ (-1, 0)
      clear2  = occ (-1, 1)
      
      match = up && (not down) && (not left)
       
  in if | match && clear2 -> Just {dest = (-1, 1), distance = 180}
        | match           -> Just {dest = (-1, 0), distance = 90}
        | otherwise -> Nothing
        
        
matchCcw : (Vec -> Bool) -> Maybe Match
matchCcw occ = 
  let left    = occ (-1, 0)
      right   = occ (1, 0)
      up      = occ (0, 1)
      clear2     = occ (0, 2)
      
      match = left && (not right) && (not up)
       
  in if | match && up2  -> Just {dest = (-1, 1), distance = 180}
        | match         -> Just {dest = (0,  1), distance = 90}
        | otherwise -> Nothing        
        

-- relRotate : Coord -> Model -> Int -> Vec -> Vec -> Maybe Rotation
-- relRotate pos model corner r u = 
--   let occ x y = occupied model (rel x y) 
--       rel x y = pos `add` (r `scale` x) `add` (u `scale` y)
--       
--       top   = occ 0 1 
--       right = occ 1 0
--       left  = occ -1 0
-- 
--       pivot = if left then corner else (corner + 1) % 4
--       dir = if left then -1 else 1
--  
--       far = occ dir 1
--       dest = if (not far) then rel dir 1 else rel 0 1
--       
--       rotation = {rotation = 0, final = if far then 90 else 180, 
--            clockwise = right, pivot = pivot, dest = dest}
--       
--   in bool (left `xor` right && (not top))  rotation
-- 
-- 
-- 
-- tryRotate : Coord -> Model -> Dir -> Maybe Rotation
-- tryRotate pos model dir = let rotate = relRotate pos model in 
--   case dir of 
--     UpDir    -> rotate 0 {x = 1,  y = 0}  {x = 0, y = -1} 
--     RightDir -> rotate 1 {x = 0,  y = 1} {x = 1, y = 0} 
--     DownDir  -> rotate 2 {x = -1, y = 0}  {x = 0, y = 1} 
--     LeftDir  -> rotate 3 {x = 0,  y = -1}  {x = -1, y = 0} 



-- tryRotation : Model -> Dir -> Square -> Square
-- tryRotation model dir square = {square | rotation <- concatMaybes square.rotation (tryRotate square.pos model dir)}
  
  
setSquare : SquareId -> (Square -> Square) -> Model -> Model
setSquare k f model = {model | squares <- Dict.update k (Maybe.map f) model.squares}


-- rotateEvent : Dir -> SquareId -> (Model -> Model)
-- rotateEvent dir k model = setSquare k (tryRotation model dir) model


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


      