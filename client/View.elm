
module View where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import States exposing (..)

import Port exposing (Message)
import JavaScript.Decode as Dec


onEnter : Message -> Attribute
onEnter message =
    on "keydown"
      (Dec.customDecoder keyCode is13)
      (always message)

is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"


-- Html view code
view : SendAction -> Model -> Html
view sendUpdate model = let
  
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
      
    waitingView : Html
    waitingView = text "Waiting for connection..."

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

  in case model of
    Login l   -> loginView  l
    Playing p -> playingView p
    NotConnected -> waitingView


