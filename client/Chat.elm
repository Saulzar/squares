import Html (..)
import Html.Attributes (..)
import Html.Events (..)

import Html

import List 

import Dict (Dict)
import Dict

import Maybe
import Maybe (..)

import Debug
import String

import Signal
import WebSocket
  
import Util  



import Json.Encode as Enc
import Json.Decode as Dec

import Json.Decode (object2, object1, (:=), int, string)

type alias UserName = String
type alias UserId = Int
  
type alias Users = Dict UserId UserName
type alias ChatLog = {sender : UserName, message : String, status : Maybe Bool}


type ClientMessage = ClientChat String | ClientLogin String | ClientHello

type alias PlayingState = 
  { id : UserId
  , users :  Users
  , log : List ChatLog
  , chat : String
  }
  

type alias LoginState = 
  { error : Maybe String
  , waiting : Bool
  , login   : String
  }
  
  
type Model = Login LoginState  | Playing PlayingState


type ServerMessage 
    = Connected Int String
    | Disconnected Int
    | Chat Int String
    | Error String
    | Welcome Int Users



type Action 
  = ServerMessage ServerMessage 
  | UpdateChat String
  | SubmitChat 
  
  | UpdateLogin String
  | SubmitLogin 

  
update : Action -> (Model, Maybe ClientMessage) -> (Model, Maybe ClientMessage)
update action (model, _) = Debug.log (toString action) (case model of
  Playing p -> updatePlaying action p
  Login l   -> updateLogin action l )

  
no : a -> (a, Maybe b)
no a = (a, Nothing)

out : a -> b -> (a, Maybe b)
out a b = (a, Just b)

updateLogin : Action -> LoginState -> (Model, Maybe ClientMessage)
updateLogin action l = case action of
  ServerMessage message -> case message of 
    Welcome id users -> no (login id)
    _          -> no (Login l)
    
  UpdateLogin login   -> no (Login  {l | login   <- login})
  SubmitLogin         -> out (Login {l | waiting <- True}) (ClientLogin l.login) 
  
  _  -> no (Login l)



                           

noP : PlayingState -> (Model, Maybe ClientMessage)
noP p = (Playing p, Nothing)

updatePlaying : Action -> PlayingState -> (Model, Maybe ClientMessage)
updatePlaying action p =  case action of
  ServerMessage message -> (serverMessage message p, Nothing)
  SubmitChat           -> out (Playing {p | chat <- ""}) (ClientChat p.chat)
  UpdateChat msg       -> noP {p | chat <- msg}
  _                    -> noP p


serverMessage : ServerMessage -> PlayingState  -> Model
serverMessage message p =  case message of  
    Connected id name    -> Playing p
    Disconnected id        -> Playing p
    Chat id message      -> Playing p
    Error reason         -> Playing p
    _ -> Playing p
       
       
initial : Model
initial = Login 
  { error = Nothing
  , waiting = False
  , login = ""
  }
  
login : Int -> Model
login id = Playing
  { id = id
  , users = Dict.empty
  , log = []
  , chat = ""
  }


inputs : Signal (Maybe Action)
inputs = Signal.mergeMany 
  [ Signal.subscribe updateChan
  , Signal.map Just serverMessages 
  ]



                     
-- manage the model of our application over time
outputs : Signal (Model, Maybe ClientMessage)
outputs = Signal.foldp (Util.maybe update) (initial, Just ClientHello) inputs
    

    
-- updates from user input
updateChan : Signal.Channel (Maybe Action)
updateChan = Signal.channel Nothing    
 
 
sendUpdate : Action -> Signal.Message
sendUpdate action = Signal.send updateChan (Just action)


clientMessage : Maybe ClientMessage -> String
clientMessage mm = case mm of 
    Nothing   -> ""
    Just msg  -> encodeMessage msg


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
  [ decObj "connected" (object2 Connected 
        ("id" := int) ("name" := string))
  , decObj "disconnected" (object1 Disconnected 
        ("id" := int))
  , decObj "chat" (object2 Chat 
        ("sender" := int) ("message" := string))
  , decObj "error" (object1 Error 
        ("reason" := string))
  , decObj "connect" (object2 Welcome 
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
decodeMessage str = Debug.log "asdf" (Err "bad") {-Dec.decodeString messageDecoder str-}

decodeAction : String -> Action
decodeAction str = ServerMessage <| case (decodeMessage str) of
  Ok msg  -> msg
  Err err -> Error (String.concat ["decode error: ", err]) 


clientMessages : Signal String
clientMessages = Signal.map clientMessage << Signal.keepIf Util.isJust Nothing << Signal.map snd <| outputs

serverMessages : Signal Action
serverMessages = Signal.map decodeAction (WebSocket.connect "ws://localhost:9160" clientMessages)


view : Model -> Html
view model = case model of
  Login l   -> loginView l
  Playing p -> playingView p


loginView : LoginState -> Html
loginView l = div []
  [ input 
      [ placeholder "Name here"
      , value l.login
      , on "input" targetValue (sendUpdate << UpdateLogin)
      , disabled l.waiting
      ] []
  , button 
    [ onClick (sendUpdate SubmitLogin)
    , disabled l.waiting] 
    [text "Join"]
  ] 


playingView : PlayingState -> Html
playingView p = div []
  [ showInput (p.chat)
  , showLog (p.log)
  ]

  
onEnter : Signal.Message -> Attribute
onEnter message =
    on "keydown"
      (Dec.customDecoder keyCode is13)
      (always message)

is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"


  
showLog : List ChatLog -> Html
showLog chats = div [] (List.map showChat chats)

showChat : ChatLog -> Html
showChat chat = p [] [ text chat.sender, text chat.message ]
                

showInput : String -> Html
showInput chat = input
  [ placeholder "Send a message"
  , value chat
  , on "input" targetValue (sendUpdate << UpdateChat)
  , onEnter (sendUpdate SubmitChat)
  ] []
  
    
main = Signal.map view (Signal.map fst outputs)  





      