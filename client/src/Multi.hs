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
makeLogin name = Login (T.pack name)


loginBox :: (MonadWidget t m) => m (Event t Login)
loginBox = do
  
  text "Chose a name: "
  t <- textInput
  go <- button "Go!"
  return $ fmap makeLogin $ tag (current $ _textInput_value t) go
  
  
connectionState :: (MonadWidget t m) => ConnectionState -> m (Event t ConnectionState)
connectionState (ConnectState url) = do
  connected <- once url >>= openConnection
  return (fmap LoginState connected)

connectionState (LoginState conn) = do
  
  login <- loginBox
  result <- tryLogin (fmap (, conn) login)
  
  return $ ffor result $ \r -> case r of 
      Left  err     -> ErrorState $ showLoginError err
      Right success -> PlayingState (conn, success)
      
      
connectionState (PlayingState _) = return never
connectionState (ErrorState msg) = do
  text $ T.unpack msg 
  return never
      

showLoginError :: LoginError -> Text
showLoginError LoginFull        = "Game full"
showLoginError (LoginDataError _) = "Data error between client/server"


disconnectError :: WS.ConnClosing -> Text
disconnectError _ = "Connection lost"

  
multiPlayer :: (MonadWidget t m) =>  (Connection, (UserId, Game)) -> m ()
multiPlayer (conn, (uid, game)) = do
  
  text $ "Ta daaah!" ++ show (uid, game)
  
  return ()
  
windowView   :: forall t m. (MonadWidget t m) =>  m ()
windowView = do
  rec
    stateEvent <- fmap switchPromptlyDyn $ 
        widgetHold (connectionState initial) (fmap connectionState transitions)
        
    let transitions = leftmost
          [ fmap (ErrorState . disconnectError) disconnectEvent
          , stateEvent
          ]
    
    let connected = fmapMaybe (^? _LoginState) stateEvent
        loggedIn  = fmapMaybe (^? _PlayingState) stateEvent 
    
    disconnectEvent <- pollConnection pollRate $ connected
    widgetHold (return ()) $ fmap multiPlayer loggedIn
 
  return ()
 
  where
  
    url = "ws://0.0.0.0:9160" :: Text 
    initial = ConnectState url
    pollRate = 100000

  
main :: IO ()
main = mainWidgetWithCss $(embedFile "style.css") $ el "div" $ windowView
  
  

