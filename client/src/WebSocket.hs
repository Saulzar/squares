

module WebSocket 
  ( openConnection
  , holdConnection
  , receiveData
--   , receiveText

  , sendMessage      
  , sendData
  , sendText
  
  , socket_message
  , socket_decodeFail
  , socket_disconnected

  , closeConnection   
  , closeConnections
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
import Data.Traversable

import Control.Concurrent 

import qualified JavaScript.WebSockets as WS
import qualified JavaScript.WebSockets.Internal as WS


import JavaScript.WebSockets (Connection, SocketMsg, WSReceivable, WSSendable)


-- Utility functions


-- Wrapper for performEventAsync where the function is called in another thread
forkEventAsync :: MonadWidget t m => Event t a -> (a -> (b -> IO ()) -> IO ()) -> m (Event t b)
forkEventAsync e f = performEventAsync $ ffor e $ \a cb -> liftIO $ do 
    void $ forkIO $ f a cb
    

    
performAsync ::  MonadWidget t m => Event t a -> (a -> IO b) -> m (Event t b)
performAsync e f = forkEventAsync e (\a cb -> f a >>= cb) 


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
  

counter :: MonadWidget t m => Event t a -> m (Dynamic t Int)
counter event = foldDyn (+) 0 (fmap (const 1) event)

attachCounter :: MonadWidget t m => Event t a -> m (Event t (Int, a))
attachCounter event = do
  count <- counter event
  return $ attachDyn count event

  
delay :: MonadWidget t m => Int -> Event t a -> m (Event t a)
delay n event = performAsync event (\a -> threadDelay n >> return a)


splitEvents :: (Reflex t) => Event t Int -> Event t (Int, a) -> Event t (Event t a)
splitEvents ids events = fmap (\i -> fmap snd $ ffilter ((==i) . fst) events) ids   


switchEvents :: (MonadWidget t m) => Event t Int -> Event t (Int, a) -> m (Event t a)
switchEvents ids events = switchPromptly never (splitEvents ids events)


iterateAsync :: (MonadWidget t m) => Event t a ->  (a -> IO (Maybe (a, b))) -> m (Event t b)  
iterateAsync trigger f = do
  
  rec
    count <- counter trigger 
    event <- performAsync (attachDyn count $ leftmost [trigger, fmap fst result])  f'
    result <- fmap (fmapMaybe id) $ switchEvents (updated count) event
    
  return (fmap snd result)
  
  where
    f' (i, a) = f a >>= \a' -> return (i, a')
    
repeatAsync :: (MonadWidget t m) => Event t a -> (a -> IO b) -> m (Event t b)  
repeatAsync trigger f = iterateAsync trigger f' 
  where
    f' a = f a >>= \b -> return $ Just (a, b)

  
tickOn :: (MonadWidget t m, Show a) => Int -> Event t a -> m (Event t a)
tickOn n trigger = repeatAsync trigger $ (\a -> threadDelay n  >> return a)
  

tick :: MonadWidget t m => Int -> m (Event t ())
tick n = getPostBuild >>= tickOn n


 
--Open connections for each url provided
openConnection :: MonadWidget t m => Event t Text -> m (Event t Connection)
openConnection request = forkEventAsync request  $ \url cb -> do
      WS.openConnection url >>= cb
      
      
holdConnection :: MonadWidget t m => Event t Text -> m (Dynamic t (Maybe Connection))
holdConnection request = do
  open <- openConnection request
  holdDyn Nothing (fmap Just open)
      
--Receieve messages using the latest connection 
receiveMessage :: (MonadWidget t m) => Dynamic t (Maybe Connection) -> m (Event t (Maybe SocketMsg))
receiveMessage conn = iterateAsync (updated conn) $ 

  -- On the first time through a Nothing message is returned (and Nothing connection), second time Nothing for the whole
  traverse $ \conn' -> do 
    msg  <- WS.receiveMessageMaybe conn'
    return (fmap (const conn') msg, msg)

      
--Record type for labelling the various events returned
data SocketEvents t a = SocketEvents
  { socket_message      :: Event t a
  , socket_disconnected :: Event t ()
  , socket_decodeFail   :: Event t SocketMsg 
  }
      
      
      

            
--using the ghcjs-websockets WSReceivable class decode a socket message
unwrapData ::  (Reflex t, Binary a) => Event t (Maybe SocketMsg) -> SocketEvents t a
unwrapData msgEvent = SocketEvents msg disc decFail where
    disc = fmap (const ()) $ ffilter isNothing msgEvent
    (decFail, msg) = splitEither $ fmap WS.unwrapReceivable (fmapMaybe id msgEvent)

-- Hack because of broken ghcjs-websockets Binary a => WSReceivable a instance 
-- unwrapText ::  (Reflex t) => Event t (Maybe SocketMsg) -> SocketEvents t Text
-- unwrapText msgEvent = SocketEvents msg disc decFail where
--     disc = fmap (const ()) $ ffilter isNothing msgEvent
--     (decFail, msg) = splitEither $ fmap WS.unwrapReceivable (fmapMaybe id msgEvent)
    
    
    
--Main receive API functions, returns SocketEvents record
receiveData  :: (MonadWidget t m, Binary a) => Dynamic t (Maybe Connection) -> m (SocketEvents t a)
receiveData conn = fmap unwrapData $ receiveMessage conn

-- receiveText  :: (MonadWidget t m) => Event t Connection -> m (SocketEvents t a)
-- receiveText connEvent = fmap unwrapText $ receiveMessage connEvent


performConnection_ :: (MonadWidget t m) => (Connection -> a -> IO b) -> Dynamic t (Maybe Connection) -> Event t a -> m ()
performConnection_ f conn event = performEvent_ $ fmap perform (attach (current conn) event) where
    perform (mayConn, a) = liftIO $ for_ mayConn $ \conn -> void $ f conn a
      
      
sendMessage :: (MonadWidget t m) => Dynamic t (Maybe Connection) -> Event t SocketMsg -> m ()
sendMessage = performConnection_ WS.sendMessage

sendText :: (MonadWidget t m) => Dynamic t (Maybe Connection) -> Event t Text -> m ()
sendText = performConnection_ WS.sendText

sendData :: (MonadWidget t m, Binary a) => Dynamic t (Maybe Connection) -> Event t a -> m ()
sendData = performConnection_ WS.sendData

closeConnection :: (MonadWidget t m) => Dynamic t (Maybe Connection) -> Event t () -> m ()
closeConnection = performConnection_ (\conn () -> WS.closeConnection conn)
    

-- Close old connections, leaving only the most recently received connection open
closeConnections :: (MonadWidget t m) => Dynamic t (Maybe Connection) ->  m ()   
closeConnections conn =  do
  performEvent_ $ ffor (fmapMaybe id $ tag (current conn) (updated conn)) $ liftIO . WS.closeConnection

 