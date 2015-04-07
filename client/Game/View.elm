module Game.View where


import Game.Interface exposing (SendAction)
import Game.Interface as Interface

import Game.Game exposing (Point, Square, SquareId, Dir(..), Coord)
import Game.Game as Game

import Util exposing (..)

import Html exposing (Html)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)

import Dict
import String

squareColour : Interface.Model -> SquareId -> String
squareColour i s = let selectedCol = "skyblue"
                       defaultCol = "royalblue"
                   in if (i.selected == Just s) then selectedCol else defaultCol

showPivot : (Coord, Int) -> Html
showPivot (pivot, _) = circle ([fill "#000000", r "0.1"] ++ centre pivot) []

rotation : (Coord, Int) -> Svg.Attribute
rotation (pivot, deg) = rotate deg pivot


view : SendAction -> Interface.Model -> Html
view sendUpdate m = 
  let squares = List.map (showSquare sendUpdate m) (Dict.toList m.model.squares)
  in svg [ version "1.1", x "0", y "0", viewBox "0 0 20 10" ] squares

    
-- main = Signal.map view model  



showSquare : SendAction -> Interface.Model -> (SquareId, Square) -> Html
showSquare sendUpdate i (k, sq) = 
  let square = rectAt sq.pos 1 1
        ([ fill (squareColour i k),
           stroke "black",
           strokeWidth "0.02",
          onClick (sendUpdate (Interface.Select k))
        ] ++ catMaybes [rotation `Maybe.map` (Game.rotation sq)])
        
      pivot    = showPivot `Maybe.map` (Game.rotation sq)
  in
    g [] (catMaybes [Just square, pivot])
    
    
rectAt : Point a -> Int -> Int -> List Attribute -> Html
rectAt pos w h attrs = rect (width (toString w) :: height (toString h) :: position pos ++ attrs) []


position : Point a -> List Attribute
position p = [x (toString p.x), y (toString p.y)]
      
centre : Point a -> List Attribute
centre p = [cx (toString p.x), cy (toString p.y)]

rotate : b -> Point a -> Attribute
rotate deg centre = transform (String.concat ["rotate (", toString deg, ",", toString centre.x, ",", toString centre.y, ")"])
       