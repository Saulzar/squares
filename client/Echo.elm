module Main (main) where

import Html exposing (Html, p, text, div)
import Port exposing (Port, InboundPort, OutboundPort)


import Task 
import String


port incoming : InboundPort String
port errors : InboundPort String


type alias Message = Result String String

messages : Stream Message
messages = Stream.merge 
  (Stream.map Ok incoming.stream) 
  (Stream.map Err errors.stream)
  

responses : Stream String
responses = Stream.map String.toUpper incoming.stream 

port outgoing : OutboundPort String
perform Task.subscribe responses (Port.send outgoing.address)




showMessage : Message -> Html
showMessage msg = 
  let t = case msg of
    Err err ->  String.concat ["error: ", err]
    Ok  msg -> msg
  in p [] [text t]


incomingLog : Signal (List Message)
incomingLog = Stream.fold (::) [Ok "testing"] messages


showLog : List Message -> Html
showLog messages = div [] (List.map showMessage messages)





main : Signal Html
main = Signal.map showLog incomingLog





      