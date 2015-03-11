

module Model where

import List 
import Dict (Dict)

import Dict
import Maybe
import Signal
 


type alias Point a = { x:a, y:a }
type alias Coord = Point Int



type alias Moving = { rotation:Int, final:Int, clockwise: Bool, pivot:Int, dest:Coord }
type alias Square = { pos:Coord, moving:Maybe Moving}

type alias SquareId = Int


type alias Model = { squares:Dict SquareId Square, max: Coord }



initial : Model
initial = let squares = List.map2 (,) [1..10] coords 
              coords  = List.map squareAt [ (1, 1), (3, 3), (2, 1), (4, 4), (5, 5) ]
        in
          { squares =  Dict.fromList squares,  max = { x = 10, y = 10 }}
  
  
  
setSquare : Model -> SquareId -> (Square -> Square) -> Model
setSquare model k f = {model | squares <- Dict.update k (Maybe.map f) model.squares}


animateSquare : SquareId -> Square -> Square
animateSquare k s = s

animate : Model -> Model
animate  model = {model | squares <- Dict.map animateSquare model.squares}
  


squareAt : (Int, Int) -> Square
squareAt (x, y) = { pos = {x = x, y = y}, moving = Nothing }



      