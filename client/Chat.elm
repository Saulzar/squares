module Main (main) where 

import Dict exposing (Dict)

import Maybe
import Maybe exposing (andThen)

import Debug
import String
 
import Task exposing (Task, ThreadID)
import Port exposing (Port, InboundPort, OutboundPort)


import States exposing (..)
import Util  


inputs : Stream Action
inputs = Stream.mergeMany 
  [ updatePort.stream
  , serverMessages 
  ]

                     
-- manage the model of our application over time
outputs : Signal (Model, Maybe ClientMessage)
outputs = Stream.fold update (initial, Just ClientHello) inputs
    

port updatePort : Port Action

sendUpdate : Action -> Message
sendUpdate action = Port.send updatePort.address action



clientMessages : Stream String
clientMessages = Stream.map encodeMessage << Stream.filterMap snd <| outputs

-- Websocket communication (via Javascript ports)
port incoming : InboundPort String
port errors : InboundPort String


type alias Message = Result String String

serverMessages : Stream Action
serverMessages = Stream.merge 
  (Stream.map decodeAction incoming.stream) 
  (Stream.map Error errors.stream)


port outgoing : OutboundPort String
perform Task.subscribe clientMessages (Port.send outgoing.address)



  
    
main = Signal.map view (Signal.map fst outputs)  

