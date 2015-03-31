module Main (main) where 

import Dict exposing (Dict)

import Maybe
import Maybe exposing (andThen)

import Debug
import String
 
import Task exposing (Task, ThreadID)
import Port exposing (Port, InboundPort, OutboundPort, Message)


import States exposing (..)
import Encode exposing (..)
import View exposing (..)

import Util  


inputs : Stream Action
inputs = Stream.mergeMany 
  [ updatePort.stream
  , serverMessages 
  ]

                     
-- manage the model of our application over time
outputs : Signal (Model, Maybe ClientMessage)
outputs = Stream.fold update (initial, Nothing) inputs
    

port updatePort : Port Action

sendUpdate : Action -> Port.Message
sendUpdate action = Port.message updatePort.address action



clientMessages : Stream String
clientMessages = Stream.map encodeMessage << Stream.filterMap snd <| Signal.toStream outputs

-- Websocket communication (via Javascript ports)
port incoming : InboundPort String
port connected : InboundPort Bool
port errors : InboundPort String


type alias Message = Result String String

serverMessages : Stream Action
serverMessages = Stream.mergeMany
  [ Stream.map decodeAction incoming.stream
  , Stream.map (ServerMessage << Error) errors.stream
  , Stream.map (always (ServerMessage Connected)) connected.stream
  ]

port outgoing : OutboundPort String
perform Task.subscribe clientMessages (Port.send outgoing.address)

    
main = Signal.map (view sendUpdate) (Signal.map fst outputs)  

