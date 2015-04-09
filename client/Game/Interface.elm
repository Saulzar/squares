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

  
type alias KeyMap = Dict KeyCode Binding
type alias Model = { model : Game.Model, selected : Maybe SquareId,  keyMap : KeyMap }


type Input = Select SquareId | KeyDown KeyCode | Animate Int
type Binding = GoUp | GoLeft | GoRight | GoDown

type alias SendInput = Input -> Port.Message
  
  
update : Input -> Model -> (Model, Maybe Game.Event)
update input m = 
  case input of  
    Animate dt -> (animateModel dt m, Nothing)
    Select sq -> (select sq m, Nothing)
    KeyDown key -> (m, Dict.get key (m.keyMap) `andThen` gameInput m)
            
       
          
mapGame : (Game.Model -> Game.Model) -> Model -> Model
mapGame f m = {m | model <- f m.model}
      
gameEvent : Game.Event -> Model ->  Model
gameEvent event = mapGame (Game.runEvent event)

animateModel : Int -> Model -> Model
animateModel dt = mapGame (Game.animate dt)

gameInput : Model -> Binding -> Maybe Game.Event
gameInput m binding = 
  let tryRotate dir = m.selected `andThen` \id -> Game.rotateDir m.model id dir
  in case binding of
    GoUp    -> tryRotate UpDir
    GoLeft  -> tryRotate LeftDir 
    GoRight -> tryRotate RightDir
    GoDown  -> tryRotate DownDir

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