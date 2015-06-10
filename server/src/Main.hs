{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception.Lifted (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.ByteString.Lazy.Char8 as B
import Data.ByteString.Lazy.Char8 (ByteString)

import qualified Data.Map as M
import Data.Foldable

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import Control.Concurrent.STM 
import Control.Lens

import Control.Arrow ((&&&))

import qualified Network.WebSockets as WS

import Data.Binary
import GHC.Generics

import Squares.Types
import qualified Squares.Game as G


  
data ServerState = ServerState 
  {  _server_connections :: M.Map UserId WS.Connection
  ,  _server_game  :: Game
  ,  _server_events :: [UserEvent] 
  } 
  
 
$(makeLenses ''ServerState)

-- Create a new, initial state:

newServerState :: ServerState
newServerState = ServerState 
  { _server_connections = M.empty
  , _server_game = G.initialGame
  , _server_events = []
  }
  
  

type Server a = ReaderT (TVar ServerState) IO a


serverState :: Server ServerState
serverState = ask >>= liftIO . readTVarIO

-- Send a message to all clients, and log it on stdout:


withState :: (TVar ServerState -> STM a) -> Server a
withState f = do
  stateVar <- ask
  liftIO $ atomically $ do
    f stateVar


modifyState :: (ServerState -> (ServerState, a)) -> Server a
modifyState f = withState $ \stateVar -> do
    (state', a) <- readTVar stateVar >>= return . f
    writeTVar stateVar state'
    return a
    

modifyState_ :: (ServerState -> ServerState) -> Server ()
modifyState_ f = modifyState (\state -> let state' = f state in (state', () ))



runEvent :: UserEvent ->  ServerState -> ServerState
runEvent e state = state & server_game %~ G.runEvent e

                                        
                    
                    
tryLogin :: (Login, WS.Connection) -> ServerState -> (ServerState, (Maybe UserEvent, LoginResponse))
tryLogin (Login user, conn) state = case maybeAdd of 
    Nothing    -> (state, (Nothing, loginError LoginFull))
    Just e -> state & server_connections %~ M.insert (fst e) conn 
                    & runEvent e 
                    & \state' ->  (state', (Just e, loginOk (fst e, state'^.server_game)) )
  where                      
      maybeAdd =  G.addUser user $ state^.server_game 
         
-- Remove a client:

clientDisconnect :: UserId -> ServerState -> (ServerState, UserEvent)
clientDisconnect i state = state & server_connections %~ (M.delete i) 
                                 & runEvent e
                                 & (, e)

  where
    e = (i, ev)
    ev = if state ^. server_game . game_started 
               then UserDisconnect else UserLeave



broadcast :: (Binary a, Show a) =>  a -> Server ()
broadcast a = do
    liftIO $ print a
    users <- fmap (^. server_connections) serverState
    
    for_ users $ \conn -> 
      send conn a 




    
send :: (MonadIO m, Binary a, Show a) => WS.Connection -> a -> m ()
send conn msg = liftIO $ do
  putStrLn ("sending: " ++ show msg)
  WS.sendBinaryData conn str 
    where
      str = encode msg

eitherDecode :: Binary a => ByteString -> Either Text a
eitherDecode b = case decodeOrFail b of
    Left (_, _, e)  -> Left (T.pack e)
    Right (_, _, x) -> Right x
      
receive :: (MonadIO m, Binary a, Show a) => WS.Connection -> m (Either Text a)
receive conn = liftIO $ do
  str <- WS.receiveData conn
  let r = eitherDecode str
  putStrLn ("received: " ++ show r)
  return r
       
       


-- serverError :: (MonadIO m) => WS.Connection -> String -> m ()
-- serverError conn err = send conn (ServerError (T.pack err))
  
    
gameReset ::  ServerState -> ServerMsg
gameReset state = ServerReset (state^.server_game) 
 
  
runUser :: (UserId, WS.Connection) -> Server ()
runUser (i, conn) = flip finally disconnect $ runUser'    

  where 
    
    runUser' = do
      received <- receive conn
      either onErr onCmd received

    runEvent' e = do 
      modifyState_ $ runEvent (i, e)
      broadcast (ServerEvent (i, e))
    
    onCmd cmd = do 
      case cmd of
        ClientChat msg    -> runEvent' (ChatEvent msg)
        ClientMove move   -> return () --TODO 
        ClientFrame ->  return ()
      
        
      
    onErr err = send conn (ServerError $ T.concat ["Decode error: ", err])
    
    disconnect = do
      e <- modifyState $ clientDisconnect i
      broadcast (ServerEvent e)
    
-- newUser :: Client -> Server ()
-- newUser  client = flip finally disconnect $ do  
--   
--   
--   sendClient client (gameState client state) 
--   modifyState $ addClient client
--   
-- 
--   broadcast $ Connected i (client ^. client_user . user_name) 
--   runClient client
-- 
--   where   
--     i = client ^. client_id
--     disconnect = do
--       modifyState $ removeClient i
--       broadcast $ Disconnected i
    

runLogin :: WS.Connection -> Server ()
runLogin conn = do
  received <- receive conn
  either onErr onLogin received
  
  where
    onLogin msg = case msg of 
      login -> do 
        (ev, response) <- modifyState (tryLogin (login, conn))
        send conn response
        traverse_ broadcast ev
        
        case response of 
             Right (i, _)  -> runUser (i, conn)
             _             -> return ()
        
    onErr err = send conn (loginError $ LoginDataError $ T.concat ["Decode error: ", err])
    
    



   
port :: Int
port = 9160

ip :: String
ip = "0.0.0.0"

application :: TVar ServerState -> WS.ServerApp
application stateVar pending = do
    
  print (WS.pendingRequest pending)  
    
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30

  flip runReaderT stateVar $ runLogin conn

  


main :: IO ()
main = do
  stateVar <- newTVarIO newServerState
  
  putStrLn $ "squares-server listening on "++ ip ++ ":" ++ show port
  WS.runServer ip port $ application stateVar      
    
      