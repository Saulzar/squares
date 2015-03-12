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
import Model (Point, Square, Model, SquareId, Dir(..))

import Set (Set)
import Set

import Keyboard (KeyCode)
import Time (..)
  
  
type alias KeyMap = Dict KeyCode Event
type alias Interface = { model : Model, selected : Maybe SquareId,  keyMap : KeyMap }
  
  
  
  
type Action = Select SquareId | KeyDown KeyCode
type Event = GoUp | GoLeft | GoRight | GoDown
  
  
update : Action -> Interface -> Interface
update action interface =
  case action of  
    Select sq -> select sq interface
    KeyDown key -> 
      maybe modelEvent 
        (Dict.get key (interface.keyMap) `andThen` 
        updateEvent interface)
        interface
      
      
modelEvent : Model.Event -> Interface -> Interface
modelEvent event interface = { interface | model <- Model.event event interface.model }



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



                     
-- manage the model of our application over time
model : Signal Interface
model = Signal.foldp (maybe update) initial (Signal.subscribe updates)    
    
    
-- updates from user input
updates : Signal.Channel (Maybe Action)
updates = Signal.channel Nothing    
 
 
sendUpdate : Action -> Signal.Message
sendUpdate action = Signal.send updates (Just action)


view : Interface -> Html
view interface =
  svg [ version "1.1", x "0", y "0", viewBox "0 0 10 5" ]
  (List.map (square interface) (Dict.toList interface.model.squares))
    
    
main = Signal.map view model  


squareColour : Interface -> SquareId -> String
squareColour i s = let selectedCol = "#7FD13B"
                       defaultCol = "#4E9A06"
                   in if (i.selected == Just s) then selectedCol else defaultCol
                    
                    
square : Interface -> (SquareId, Square) -> Html
square i (k, sq) = rect
      ([ fill (squareColour i k), width "1", height "1"
      , transform "matrix(1 0 0 1 0 0)"
      , onClick (sendUpdate (Select k))
      ] ++ position sq.pos)
      []    

position : Point Int -> List Attribute
position p = [x (toString p.x), y (toString p.y)]
      