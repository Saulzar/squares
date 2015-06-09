{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TupleSections #-}
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
import qualified Data.Text as T

import Dom
import JavaScript.WebSockets.Reflex.WebSocket
import qualified JavaScript.WebSockets as WS



import Game.View
import Game.Types

import Event
import Dom       
  

  
data ConnectionState 
    = ConnectState Text
    | LoginState Connection
    | PlayingState (Connection, (UserId, Game))
    | ErrorState !Text
  

liftM concat $ do
  prisms <- mapM makePrisms [ ''ConnectionState ]
  return (prisms)  
    
tryLogin :: (MonadWidget t m) => Event t (Login, Connection) -> m (Event t LoginResponse)
tryLogin e = do
    
    r <-performAsync e $ \(login, conn) -> do
      WS.sendData conn login    
      WS.receiveDataMaybe conn

    return (filterMaybes r)    
  
  
makeLogin :: String -> Login
makeLogin name = Login user where
  user = User (T.pack name)

loginBox :: (MonadWidget t m) => m (Event t Login)
loginBox = do
  
  text "Chose a name: "
  t <- textInput
  go <- button "Go!"
  return $ fmap makeLogin $ tag (current $ _textInput_value t) go
  
  
stateView :: (MonadWidget t m) => ConnectionState -> m (Event t ConnectionState)
stateView (ConnectState url) = do
  connected <- once url >>= openConnection
  return (fmap LoginState connected)

stateView (LoginState conn) = do
  
  login <- loginBox
  result <- tryLogin (fmap (, conn) login)
  
  return $ ffor result $ \r -> case r of 
      Left  err     -> ErrorState $ showLoginError err
      Right success -> PlayingState (conn, success)
      

showLoginError :: LoginError -> Text
showLoginError LoginFull        = "Game full"
showLoginError (LoginDataError _) = "Client/server version mismatch"


disconnectError :: WS.ConnClosing -> Text
disconnectError _ = "Connection lost"

  
multiPlayer :: (MonadWidget t m) =>  (Connection, (UserId, Game)) -> m (Event t GameEvent)
multiPlayer (conn, (uid, game)) = do
  
  
  return never
  
windowView   :: forall t m. (MonadWidget t m) =>  m ()
windowView = do
  rec
    stateEvent <- fmap switchPromptlyDyn $ 
        widgetHold (stateView initial) (fmap stateView transitions)
        
    let transitions = 
          [ stateEvent
          , fmap (ErrorState . disconnectError) disconnectEvent
          ]
    
    disconnectEvent <- pollConnection pollRate $ fmapMaybe (^? _LoginState) stateEvent
    
    gameEvent <- fmap switchPromptlyDyn $
      widgetHold (return never) $ fmapMaybe (^? _PlayingState) stateEvent
 
  return ()
 
  where
  
    url = "ws://0.0.0.0:9160" :: Text 
    initial = ConnectState url
    pollRate = 100000

  
main :: IO ()
main = mainWidgetWithCss $(embedFile "style.css") $ el "div" $ windowView
  
  

