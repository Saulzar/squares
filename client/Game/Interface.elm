module Game.Interface where

import Maybe as M exposing (andThen, withDefault)

import Game.Game as Game
import Game.Game exposing (Point, Square, SquareId, Dir(..), Coord)

import Dict exposing  (Dict)

import Set exposing  (Set)
import Set

import Keyboard exposing (KeyCode)
import Keyboard
import Time

import Debug
import String
  
import Port

  
type alias KeyMap = Dict KeyCode Event
type alias Model = { model : Game.Model, selected : Maybe SquareId,  keyMap : KeyMap }


type Input = Select SquareId | KeyDown KeyCode | Animate Int
type Event = GoUp | GoLeft | GoRight | GoDown

type alias SendInput = Input -> Port.Message
  
  
update : Input -> Model -> Model
update action m = 
  case action of  
    Animate dt -> animateModel dt m
    Select sq -> select sq m
    KeyDown key -> 
         withDefault m
          (Maybe.map (modelEvent m)
            (Dict.get key (m.keyMap) `andThen` 
            updateEvent m))
            


            
       
        
mapModel : (Game.Model -> Game.Model) -> Model -> Model
mapModel f m = {m | model <- f m.model}
      
modelEvent : Model -> Game.Event -> Model
modelEvent m event = mapModel (Game.event event) m

animateModel : Int -> Model -> Model
animateModel dt = mapModel (Game.animate dt)

updateEvent : Model -> Action -> Maybe Game.Event
updateEvent m event = case event of
  GoUp    -> Maybe.map (Game.Rotate UpDir)     m.selected
  GoLeft  -> Maybe.map (Game.Rotate LeftDir)   m.selected
  GoRight -> Maybe.map (Game.Rotate RightDir)  m.selected 
  GoDown  -> Maybe.map (Game.Rotate DownDir)   m.selected
    

select : SquareId -> Model -> Model
select k m = let selected = if (m.selected == Just k) then Nothing else Just k
                     in {m | selected <- selected}    
       

defaultKeys : KeyMap
defaultKeys = Dict.fromList [
  (38, GoUp), 
  (40, GoDown),
  (37, GoLeft),
  (39, GoRight)]       
       
       
initial : Model
initial = { model = Game.initial, selected = Nothing, keyMap = defaultKeys }        