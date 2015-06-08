{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TupleSections, OverloadedLists #-}
module Main where

import Reflex
import Reflex.Dom


import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Data.List (intersperse)

import Data.Time.Clock


import Control.Lens

import Data.Monoid 

import Control.Monad 
import Control.Monad.IO.Class

import Data.FileEmbed

import Squares.Game
import Squares.Types

import Data.Text (Text)

import Dom
import JavaScript.WebSockets.Reflex.WebSocket
import qualified JavaScript.WebSockets as WS


import qualified SinglePlayer.Types as S
import qualified MultiPlayer.Types as MP

import MultiPlayer.View


import Event
import Dom       
  

  
data ConnectionState 
    = ConnectState Text
    | LoginState Connection
    | PlayingState (Connection, (UserId, Game))
    | ErrorState !Text
      deriving Show
  
    
    
tryLogin :: (MonadWidget t m) => Event t (Login, Connection) -> m (Event t LoginResult)
tryLogin e = do
    
    r <-performAsync e $ \(login, conn) -> do
      WS.sendData conn login'     
      r <- WS.receiveDataMaybe conn

    return (filterMaybes r)    
  

loginBox :: (MonadWidget t m) => m (Event t Login)
loginBox = do
  
  text "Chose a name: "
  t <- textInput
  go <- button "Go!"
  return $ fmap Login $ tag (current _textInput_value) go
  
  
stateView :: ConnectionState -> m (Event t ConnectionState)
stateView (ConnectState url) = do
  connected <- once url >>= openConnection
  return (fmap LoginState connected)

stateView (LoginState conn) = do
  
  login <- loginBox
  result <- tryLogin (fmap (, conn) login)
  
  return $ ffor result $ \r -> case r of 
      Left  err     -> ErrorState $ loginError err
      Right success -> LoggedIn (conn, success)
      

loginError :: LoginError -> Text
loginError LoginFull      = "Game full"
loginError LoginDataError = "Client/server version mismatch"


disconnectError :: WS.ConnClosing -> Text
disconnectedError _ = "Connection lost"

  

  
  
multiPlayer :: (Connection, (UserId, Game)) -> m (Event t GameEvent)
multiPlayer (conn, (uid, game)) = do
  
  
  return never
  
windowView   :: forall t m. (MonadWidget t m) =>  m ()
windowView = do
  
  stateEvent <- fmap switchPromptlyDyn $ 
      widgetHold (stateView initial) (fmap stateView transitions)
      
  let transitions = 
        [ stateEvent
        , fmap (ErrorState . disconnectError) disconnectEvent
        ]
  
  disconnectEvent <- pollConnection $ fmap (^? _LoginState) stateEvent
  
  gameEvent <- fmap switchPromptlyDyn $
    widgetHold (return never) $ fmapMaybe (^? _PlayingState) state
  
 
  where
  
    url = "ws://0.0.0.0:9160" :: Text 
    initial = ConnectState url

  
main :: IO ()
main = mainWidgetWithCss $(embedFile "style.css") $ el "div" $ showWindow
  {-
   executable squares-multi
  hs-source-dirs: src
  main-is: Multi.hs
  build-depends:
    base,
    squares,
    reflex,
    ghcjs-dom,
    reflex-dom,
    ghcjs-websockets,
    ghcjs-websockets-reflex,
    containers,
    text,
    transformers,
    lens,
    linear,
    dependent-sum,
    file-embed,
    ghcjs-base,
    hashable,
    binary,
    time  
    
  default-extensions: OverloadedStrings, DeriveGeneric, RecursiveDo, ScopedTypeVariables, FlexibleContexts, TupleSections, TemplateHaskell, RankNTypes


  other-extensions: TemplateHaskell
  ghc-prof-options: -fprof-auto
  ghc-options: -fwarn-tabs -funbox-strict-fields -O2-}
  

