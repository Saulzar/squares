module Encode where

import Dict
import String

import States exposing (..)

import JavaScript.Encode as Enc
import JavaScript.Decode as Dec

import JavaScript.Decode exposing (object2, object1, (:=), int, string)


-- Encoding and decoding JSON messages
encodeMessage : ClientMessage -> String
encodeMessage msg = Enc.encode 0 (messageValue msg)


decObj : String -> Dec.Decoder a -> Dec.Decoder a
decObj tag dec = object1 identity (tag := dec)  


user : Dec.Decoder (UserId, UserName)
user = object2 (,) ("userId" := int) ("userName" := string)

decUsers : Dec.Decoder Users
decUsers = Dec.map Dict.fromList (Dec.list user)


messageDecoder : Dec.Decoder ServerMessage
messageDecoder = Dec.oneOf 
  [ decObj "connected" (object2 UserConnected 
        ("id" := int) ("name" := string))
  , decObj "disconnected" (object1 UserDisconnected 
        ("id" := int))
  , decObj "chat" (object2 Chat 
        ("sender" := int) ("message" := string))
  , decObj "error" (object1 Error 
        ("reason" := string))
  , decObj "welcome" (object2 Welcome 
        ("id" := int) 
        ("users" := decUsers))
  ]
    
    

encObj : String -> List (String, Enc.Value) -> Enc.Value
encObj tag attrs = Enc.object [(tag, Enc.object attrs)]    
  
messageValue : ClientMessage -> Enc.Value
messageValue msg = case msg of
  ClientChat m -> encObj "clientChat"
      [ ("message", Enc.string m) ]
  ClientLogin name -> encObj "login"
      [ ("name", Enc.string name)]
  ClientHello -> encObj  "hello" []


decodeMessage : String -> Result String ServerMessage
decodeMessage str = Dec.decodeString messageDecoder str

decodeAction : String -> Action
decodeAction str = ServerMessage <| case (decodeMessage str) of
  Ok msg  -> msg
  Err err -> Error (String.concat ["decode error: ", err]) 