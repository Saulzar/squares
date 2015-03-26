import Html (..)
import Html.Attributes (..)
import Html.Events (..)

import List 
import Dict (Dict)

import Dict
import Maybe
import Maybe (..)
import Signal

import Debug
import String
  
import Util  


import Json.Encode as Enc
import Json.Decode as Dec

type alias UserName = String
type alias UserId = Int
  
type alias Users = Dict UserId UserName
type alias ChatLog = {sender : UserName, message : String}


type ClientMessage = ClientChat String

type alias Model = 
  { id : Maybe Int
  , users :  Users
  , log : List ChatLog
  , chat : String,
  , message : Maybe ClientMessage
  }


type ServerMessage 
    = Connect Int String
    | Disconnect Int
    | Chat Int String
    | Error String



type Action 
  = ServerMessage ServerMessage 
  | UpdateChat String
  | PostChat String

  
update : Action -> Model -> Model
update action model = case action of
  ServerMessage message -> serverMessage message model
  PostChat msg         -> {model | chat <- "", message = ClientChat msg}
  UpdateChat msg       -> {model | chat <- msg}
  

clientMessage : Model -> String
clientMessage action = case action of
  PostChat chat -> 



serverMessage : ServerMessage -> Model -> Model
serverMessage message model =  case message of  
    Connect id name      -> model
    Disconnect id        -> model
    Chat id message      -> model
    Error reason         -> model
    
       
       
initial : Model
initial = { id = Nothing, users = Dict.empty, log = [], chat = "" }


inputs : Signal (Maybe Action)
inputs = Signal.subscribe updateChan


                     
-- manage the model of our application over time
model : Signal Model
model = Signal.foldp (Util.maybe update) initial inputs
    
    
-- updates from user input
updateChan : Signal.Channel (Maybe Action)
updateChan = Signal.channel Nothing    
 
 
sendUpdate : Action -> Signal.Message
sendUpdate action = Signal.send updateChan (Just action)


encodeMessage : Maybe ClientMessage -> String
encodeMessage msg = Enc.encode 0 (messageValue msg)
  
  
messageValue : ClientMessage -> Enc.Value
messageValue msg = case msg of
  ClientChat m -> Enc.object
      [ ("message", Enc.string m)
      ]


view : Model -> Html
view model =
    div []
        [ stringInput (model.chat)
        ]

  
onEnter : Signal.Message -> Attribute
onEnter message =
    on "keydown"
      (Dec.customDecoder keyCode is13)
      (always message)

is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"



stringInput : String -> Html
stringInput chat =
    input
        [ placeholder "Send a message"
        , value chat
        , on "input" targetValue (sendUpdate << UpdateChat)
        , onEnter (sendUpdate (PostChat chat))
        ]
        []
  
    
main = Signal.map view model  





      