module Websocket where

import Javascript.WebSockets

import Reflex
import Reflex.Dom

import Reflex.Dom.Class
import Reflex.Host.Class

import Data.Dependent.Sum (DSum ((:=>)))

import Control.Monad
import Control.Monad.IO.Class


websocket ::  (MonadWidget t m, WSSendable a, WSReceiveable b) => String -> Event t a -> m (Event t (Maybe b))
websocket url output = do
  
  postGui <- askPostGui
  runWithActions <- askRunWithActions
  
  conn <- openConnection url
  
  input <- newEventWithTrigger $ \trigger -> do
    liftIO $ forkIO $ do
      b <- receive conn
      postGui $ runWithActions [trigger :=> b]
    return $ closeConnection conn
    
  handle <- subscribeEvent output
  performEvent_ $ (\a -> liftIO $ send conn) output
    
  return input  
          
--     unsubscribe <- liftIO animate 
    
    
    