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
import Data.Functor

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
    
tryLogin :: (MonadWidget t m) => Connection -> Event t Login -> m (Event t LoginResponse)
tryLogin conn e = do
    
    sent     <- send e conn 
    response <- catMaybesE <$> receiveMessage (const conn <$> sent) 
    
    return $ ffor response $  \r -> case unwrapReceivable r of
        Left err -> Left (LoginDataError "Bad response from server")
        Right r  -> r
  
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
  connected <- openConnection url
  return (fmap LoginState connected)

connectionState (LoginState conn) = do
  
  login <- loginBox
  result <- tryLogin conn login
  
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


  
playingView :: (MonadWidget t m) => (Connection, (UserId, Game)) -> m ()
playingView (conn, (uid, game)) = do
  animate <- askWindow >>= animationEvent
  
  rec
    model  <- foldMany updateGame (gameModel game) actions         
    inputs <- gameView  model settings

    let settings = constant defaultSettings
        actions = {-traceEvent "actions" $-} mconcat 
          [ inputs
          , pure . const (Animate 4) <$> animate
          , pure . EventAction <$> fmapMaybe (^? _ServerEvent) incoming
          ]
    
    chatInput <- input' "input" "" (const "" <$> chat) (constDyn $ Map.empty) 
    
    let enter = ffilter (== 13) (chatInput^.textInput_keydown)
        chat  = ffilter (not . null) $ tag (current $ value chatInput) enter  
  
    let outgoing = (uid, ) <$> leftmost 
          [ ClientChat . T.pack <$> chat 
          ]
  
    send outgoing conn
    (incoming, err) <- decodeMessage <$> receiveMessages conn
    
    performEvent $ ffor incoming $ liftIO . print
    performEvent $ ffor err $ liftIO . print

  
  return ()

  
windowView   :: forall t m. (MonadWidget t m) =>  m ()
windowView = do
  rec
    stateEvent <- fmap switchPromptlyDyn $ 
        widgetHold (connectionState initial) (fmap connectionState transitions)
        
    let transitions = leftmost
          [ fmap (ErrorState . disconnectError) disconnected
          , stateEvent
          ]
    
    let connected = fmapMaybe (^? _LoginState) stateEvent
        playing  = fmapMaybe (^? _PlayingState) stateEvent 
    
    disconnected <- switchEvents (checkDisconnected pollRate) connected
    widgetSwitch $ playingView <$> playing
    
  return ()
 
  where
  
    url = "ws://0.0.0.0:9160" :: Text 
    initial = ConnectState url
    pollRate = 100000

  
main :: IO ()
main = mainWidgetWithCss $(embedFile "style.css") $ el "div" $ windowView
  
  


