
module View where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import State exposing (..)

-- Html view code
view : Report -> Model -> Html
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