import Svg (..)
import Svg.Attributes (..)
import Svg.Events (..)

import Html (Html)

import List 
import Dict (Dict)

import Dict
import Maybe
import Maybe (..)
import Signal

import Model
import Model (Point, Square, Model, SquareId, Dir(..), Coord)

import Set (Set)
import Set

import Keyboard (KeyCode)
import Keyboard
import Time

import Debug
import String
  
  
type alias KeyMap = Dict KeyCode Event
type alias Interface = { model : Model, selected : Maybe SquareId,  keyMap : KeyMap }


type Action = Select SquareId | KeyDown KeyCode | Animate Int
type Event = GoUp | GoLeft | GoRight | GoDown
  
  
update : Action -> Interface -> Interface
update action interface = 
  case action of  
    Animate dt -> animateModel dt interface
    Select sq -> select sq interface
    KeyDown key -> 
      maybe modelEvent 
        (Dict.get key (interface.keyMap) `andThen` 
        updateEvent interface)
        interface
      
      
mapModel : (Model -> Model) -> Interface -> Interface
mapModel f interface = {interface | model <- f interface.model}
      
modelEvent : Model.Event -> Interface -> Interface
modelEvent event = mapModel (Model.event event)

animateModel : Int -> Interface -> Interface
animateModel dt = mapModel (Model.animate dt)

updateEvent : Interface -> Event -> Maybe Model.Event
updateEvent interface event = case event of
  GoUp    -> Maybe.map (Model.Rotate UpDir)     interface.selected
  GoLeft  -> Maybe.map (Model.Rotate LeftDir)   interface.selected
  GoRight -> Maybe.map (Model.Rotate RightDir)  interface.selected 
  GoDown  -> Maybe.map (Model.Rotate DownDir)   interface.selected

    

maybe : (a -> b -> b) -> Maybe a -> b -> b
maybe f ma b = 
  case ma of
    Nothing -> b
    Just a  -> f a b

    
select : SquareId -> Interface -> Interface
select k interface = let selected = if (interface.selected == Just k) then Nothing else Just k
                     in {interface | selected <- selected}    

       

defaultKeys : KeyMap
defaultKeys = Dict.fromList [
  (38, GoUp), 
  (40, GoDown),
  (37, GoLeft),
  (39, GoRight)]       
       
       
initial : Interface
initial = { model = Model.initial, selected = Nothing, keyMap = defaultKeys }


keyActions : Signal (Maybe Action)
keyActions = Signal.map (\k -> Just (KeyDown k)) Keyboard.lastPressed
                     
animateActions : Signal (Maybe Action)
animateActions = Signal.map (always (Just (Animate 1))) (Time.fps 50) 
                     
inputs : Signal (Maybe Action)
inputs = Signal.mergeMany  [
    keyActions, 
    Signal.subscribe updates,
    animateActions
  ]
                     
-- manage the model of our application over time
model : Signal Interface
model = Signal.foldp (maybe update) initial inputs
    
    
-- updates from user input
updates : Signal.Channel (Maybe Action)
updates = Signal.channel Nothing    
 
 
sendUpdate : Action -> Signal.Message
sendUpdate action = Signal.send updates (Just action)


view : Interface -> Html
view interface = 
  let squares = List.map (showSquare interface) (Dict.toList interface.model.squares)
  in svg [ version "1.1", x "0", y "0", viewBox "0 0 20 10" ] squares
  
    
main = Signal.map view model  


squareColour : Interface -> SquareId -> String
squareColour i s = let selectedCol = "#7FD13B"
                       defaultCol = "#4E9A06"
                   in if (i.selected == Just s) then selectedCol else defaultCol




catMaybes : List (Maybe a) -> List a
catMaybes = List.filterMap identity

showPivot : (Coord, Int) -> Html
showPivot (pivot, _) = circle ([fill "#000000", r "0.1"] ++ centre pivot) []

rotation : (Coord, Int) -> Attribute
rotation (pivot, deg) = rotate deg pivot


showSquare : Interface -> (SquareId, Square) -> Html
showSquare i (k, sq) = 
  let square = rectAt sq.pos 1 1
        ([ fill (squareColour i k),
          onClick (sendUpdate (Select k))
        ] ++ catMaybes [rotation `Maybe.map` (Model.rotation sq)])
        
      pivot    = showPivot `Maybe.map` (Model.rotation sq)
  in
    g [] (catMaybes [Just square, pivot])
    
    
rectAt : Coord -> Int -> Int -> List Attribute -> Html
rectAt pos w h attrs = rect (width (toString w) :: height (toString h) :: position pos ++ attrs) []


position : Point a -> List Attribute
position p = [x (toString p.x), y (toString p.y)]
      
centre : Point a -> List Attribute
centre p = [cx (toString p.x), cy (toString p.y)]

rotate : a -> Point a -> Attribute
rotate deg centre = transform (String.concat ["rotate (", toString deg, ",", toString centre.x, ",", toString centre.y, ")"])
       
      