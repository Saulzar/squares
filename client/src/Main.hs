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


import Types

import Event
import View
       


runUpdate :: Action -> Model -> Model  
runUpdate (Select i) = model_selected .~ i
runUpdate (MoveAction e) = model_game %~ runMove e 
runUpdate (MadeConnection) = model_connected .~ True
runUpdate (Animate dt) = model_game %~ animate dt

  
  
  
serverInput :: Model -> ServerMsg -> Maybe Action
serverInput model msg = Nothing


tryLogin :: (MonadWidget t m) => Event t Login -> Event t Connection -> m (Event t Text, Event t (Connection, UserId))
tryLogin login connection = do
    loginDyn <- holdJust login 
    ready <- fmap filterMaybes $ attachWith (liftM2 (,)) loginDyn (fmap Just connection)  
    
    fmap splitEither $ performAsync ready $ \(login', conn) -> do
      WS.sendData conn login'     
      r <- WS.receiveDataMaybe conn
      return (r & _Right %~ (conn,))

    
   

remote :: (MonadWidget t m) => Event t Login -> Event t ClientMsg -> m (Event t ServerMsg, Event t UserId, Event t Text)
remote login outgoing = do
  rec
    
    socket <- receive (fmap fst loggedIn)
    performEvent_ $ ffor (socket_decodeFail socket) $ \msg -> 
      liftIO $ putStrLn $ "unknown socket data: " ++ show msg
      
    (loggedIn, loginErr) <- tryLogin login connection
 
    closed <- fmap unTag $ pollConnection 100000 connection 
    reconnect <- delay reconnectRate closed 
    
    let needConnection = leftmost [unTag login, reconnect]
    connection <- openConnection $ fmap (const url) needConnection

  return (socket_message socket, fmap snd loggedIn, loginErr)

  where
    url = "ws://0.0.0.0:9160" :: Text  
    reconnectRate = 1000000
    
    
  
respondServer :: (MonadWidget t m) => Dynamic t Model -> Event t ServerMsg -> Event t Action -> m (Event t ClientMsg, Event t Action)
respondServer incoming inputs = do
  
  
  --Clear the buffer once sent
--   eventBuffer <- bufferEvents gameEvent serverFrame
--   
--   --Reply with buffer of built up events
--   let reply = fmap ClientFrame $ tagDyn eventBuffer serverFrame 
  
  
  return (never, never)


    
showWindow :: forall t m. (MonadWidget t m) =>  m ()
showWindow = do
  window <- askWindow 
  animate <- animationEvent window      
  
  rec 

    model    <- foldDyn runUpdate initial actions         
    inputs <- showModel model
    
    let user = never
 
    (incoming, login, loginErr) <- remote user outgoing
    (outgoing, serverAction) <- respondServer incoming inputs
    
    
    let actions = leftmost 
          [ ffilter (nullOf move_action) inputs
          , serverAction
          , tagConst (Animate 4) animate
          ]
 
        
  return ()
  
  
  
main :: IO ()
main = mainWidgetWithCss $(embedFile "style.css") $ el "div" $ showWindow
  

