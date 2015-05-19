-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE IncoherentInstances #-}
-- {-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE OverlappingInstances #-}

module WebSocket 
  ( connect
  , receiveMessage
  , sendMessage
  , receive
  , Connection
  , SocketMsg
  , WSReceivable
  , WSSendable
  )
  where


import Reflex
import Reflex.Dom

import Reflex.Dom.Class
import Reflex.Host.Class

import Data.Dependent.Sum (DSum ((:=>)))

import Control.Monad
import Control.Monad.IO.Class
import Control.Lens

import Control.Concurrent
import Data.IORef
import Data.Text (Text)
import Data.Maybe

import Data.Binary  
import Data.Time.Clock

import Data.Foldable  
import Control.Concurrent 

import qualified JavaScript.WebSockets as WS
import qualified JavaScript.WebSockets.Internal as WS


import JavaScript.WebSockets (Connection, SocketMsg, WSReceivable, WSSendable)


-- Utility functions


-- Wrapper for performEventAsync where the function is called in another thread
forkEventAsync :: MonadWidget t m => Event t a -> (a -> (b -> IO ()) -> IO ()) -> m (Event t b)
forkEventAsync e f = performEventAsync $ ffor e $ \a cb -> liftIO $ do 
    void $ forkIO $ f a cb

-- Filter event stream with predicate and return ()
whenE_ :: (Reflex t) => (a -> Bool) -> Event t a -> Event t () 
whenE_ f = fmap (const ()) . ffilter f 

-- Split Either into two event streams
splitEither :: (Reflex t) => Event t (Either a b) -> (Event t a, Event t b)
splitEither e = (fmapMaybe (firstOf _Left) e, fmapMaybe (firstOf _Right) e)

-- Attach an event with it's previous value
pair :: MonadWidget t m => Event t a -> m (Event t (Maybe a, a))
pair event = do 
  value <- hold Nothing (fmap Just event)
  return $ attach value event
  
-- Helper function, upon receiving a connection it waits for one message and fires the event
receiveOnce  :: (MonadWidget t m) => Event t Connection -> m (Event t (Maybe (SocketMsg, Connection)))
receiveOnce connEvent = forkEventAsync connEvent $ \conn cb -> do
      msg <- WS.receiveMessageMaybe conn 
      cb $ fmap (,conn) msg 
  
  
  
-- Open connections for each url provided
connect :: MonadWidget t m => Event t Text -> m (Event t Connection)
connect request = forkEventAsync request  $ \url cb -> do
      WS.openConnection url >>= cb
      
      
-- Receieve messages using the latest connection
-- returns pair of (message, disconnected) events
receiveMessage :: (MonadWidget t m) => Event t Connection -> m (Event t SocketMsg, Event t ())
receiveMessage connEvent = do
  rec 
    message <- receiveOnce $ leftmost [connEvent, connEvent']
    let (msg, connEvent') = splitE (fmapMaybe id message)
  return (msg, whenE_ isNothing message)
      
      
-- Record type for labelling the various events returned
data SocketEvents t a = SocketEvents
  { message      :: Event t a
  , disconnected :: Event t ()
  , decodeFail   :: Event t SocketMsg 
  }
      
            
-- using the ghcjs-websockets WSReceivable class decode a socket message
unwrapMessage :: (Reflex t, WSReceivable a) =>  Event t SocketMsg -> (Event t SocketMsg, Event t a)
unwrapMessage msgEvent = splitEither $ fmap WS.unwrapReceivable msgEvent


-- Main receive API function, returns SocketEvents record
receive  :: (MonadWidget t m, WSReceivable a) => Event t Connection -> m (SocketEvents t a)
receive connEvent = do
  (msg', disc) <- receiveMessage connEvent
  let (decFail, msg) = unwrapMessage msg'
  return SocketEvents
    { message      = msg
    , disconnected = disc
    , decodeFail   = decFail
    }
      
      
sendMessage :: (MonadWidget t m) => Behavior t (Maybe Connection) -> Event t SocketMsg -> m ()
sendMessage connBhr msgEvent = performEvent_ $ fmap sendMessage (attach connBhr msgEvent) 
  
  where
    sendMessage (mayConn, msg) = 
      liftIO $ for_ mayConn $ \conn ->
           void $ WS.sendMessage conn msg
     
    

    
    