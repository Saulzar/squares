

module Single (main) where 

import Dict exposing (Dict)

import Maybe
import Maybe exposing (andThen)

import Debug
import String
 
import Keyboard 
import Time

import Port exposing (Port)

import Game.Interface as Interface
import Game.Interface exposing (Input(..), Model)

import Game.Game as Game
import Game.View as View

import Util  

      

keyActions : Stream Input
keyActions = Stream.map KeyDown Keyboard.presses
                     
animateActions : Stream Input
animateActions = Stream.map (always (Animate 1)) (Time.fps 25) 
                     
inputs : Stream Input
inputs = Stream.mergeMany  [
    keyActions, 
    updatePort.stream,
    animateActions
  ]


port updatePort : Port Input

sendUpdate : Input -> Port.Message
sendUpdate action = Port.message updatePort.address action


update : Input -> Model -> Model
update action model = 
  let (model', me) = Interface.update action model
  in Util.maybe model' (\e -> Interface.gameEvent e model') me
  
    
main = Signal.map (View.view sendUpdate) (Stream.fold update Interface.initial inputs) 