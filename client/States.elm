module States where


import Dict exposing (Dict)

import Maybe
import Maybe exposing (andThen)

import Port exposing (Message)
import Debug exposing (log)

import Game.Interface as Interface

type alias UserName = String
type alias UserId = Int

type alias Event = String

type alias Users = Dict UserId UserName

type alias PlayingState = 
  { id : UserId
  , users :  Users
  , eventLog : List Event
  , chatEntry : String
  , game : Interface.Model
  }
  

type alias LoginState = 
  { error : Maybe String
  , waiting : Bool
  , login   : String
  }
  
  
type Model = NotConnected | Login LoginState  | Playing PlayingState
type ClientMessage = ClientChat String | ClientLogin String | ClientHello


type ServerMessage 
    = UserConnected Int String
    | UserDisconnected Int
    | Chat Int String
    | Error String
    | Welcome Int Users
    | Connected
    

type Action 
  = ServerMessage ServerMessage
  | UpdateChat String
  | SubmitChat 
  | UpdateLogin String
  | SubmitLogin
  | GameInput Interface.Input


type alias ClientState = (Model, Maybe ClientMessage)
type alias SendAction = Action -> Port.Message  
 
 
 
state : (a -> Model) -> a -> ClientState
state f a = (f a, Nothing)
 
playing : PlayingState -> ClientState
playing = state Playing

login : LoginState -> ClientState
login = state Login


reply : ClientState -> ClientMessage -> ClientState
reply (m, _) msg = (m, Just msg)

-- 
-- notConnected : State ()
-- notConnected = 




  
update : Action -> ClientState -> ClientState
update action (model, _) = Debug.log (toString action) (case model of
  Playing p -> updatePlaying action p
  Login l   -> updateLogin action l
  NotConnected -> waitConnection action)
  


waitConnection : Action -> ClientState
waitConnection action  = case action of
  ServerMessage Connected -> toLogin `reply` ClientHello
  _ -> Debug.crash "waitConnection: invalid event"



updateLogin : Action -> LoginState -> ClientState
updateLogin action l = case action of
  ServerMessage (Welcome id users) ->  toPlaying id users
  
  UpdateLogin name    -> login  {l | login   <- name}
  SubmitLogin         -> login {l | waiting <- True} 
                          `reply` ClientLogin l.login
  
  _ -> Debug.crash ("updateLogin: invalid event")

                           


updatePlaying : Action -> PlayingState -> ClientState
updatePlaying action p =  case action of
  ServerMessage msg -> serverMessage msg p
  SubmitChat           -> playing {p | chatEntry <- ""} 
                          `reply`  ClientChat p.chatEntry
                          
  UpdateChat msg       -> playing {p | chatEntry <- msg}
  GameInput input       -> playing {p | game <- fst (Interface.update input p.game)} 
  
  _ -> Debug.crash ("updatePlaying: invalid event")

serverMessage : ServerMessage -> PlayingState  -> ClientState
serverMessage msg p =  case msg of
    UserConnected id name  -> playing (userConnected id name p)
    UserDisconnected id    -> playing (userDisconnected id p)
    Chat id msg      -> playing (addLog id msg p)
    Error reason         -> playing p
    _ -> Debug.crash "serverMessage: not implemented"
       

       
userConnected : UserId -> UserName -> PlayingState -> PlayingState
userConnected id name p = {p | 
    eventLog <- (name ++ " connected.") :: p.eventLog,
    users <- Dict.insert id name p.users
  }


  
withUser : UserId -> PlayingState -> (UserName -> a) -> a
withUser id p f = case (Dict.get id p.users) of
  Just user -> f user
  Nothing   -> Debug.crash ("withUser - user does not exist: " ++ toString id)  
  
  
userDisconnected : UserId -> PlayingState -> PlayingState
userDisconnected id p = withUser id p <|
  \user -> {p |
    eventLog <- (user ++ " disconnected.") :: p.eventLog,
    users <- Dict.remove id p.users
  }  
       
       
addLog : UserId -> String -> PlayingState -> PlayingState
addLog id msg p = withUser id p <|
  \user -> {p | eventLog <- userMessage user msg :: p.eventLog}
  

userMessage : UserName -> String -> Event
userMessage name msg = name ++ ": " ++ msg
       

initial : Model
initial = NotConnected
     
     
toLogin : ClientState
toLogin = login 
  { error = Nothing
  , waiting = False
  , login = ""
  }
  
toPlaying : Int ->  Users -> ClientState
toPlaying id users = playing
  { id = id
  , users = users
  , eventLog = []
  , chatEntry = ""
  , game = Interface.initial
  }
