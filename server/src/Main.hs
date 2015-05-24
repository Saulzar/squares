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
import Squares.Game


data Client = Client
  { _client_user :: User
  , _client_conn :: WS.Connection
  }
  
instance Show Client where
  show = show . _client_user

  
data ServerState = ServerState 
  {  _server_users :: M.Map UserId Client
  ,  _server_game  :: Game
  ,  _server_count :: Int
  } deriving (Show)
  
 
$(makeLenses ''ServerState)
$(makeLenses ''Client)

client_id = client_user.user_id  

-- Create a new, initial state:

newServerState :: ServerState
newServerState = ServerState 
  { _server_users = M.empty
  ,  _server_game = initialGame
  ,  _server_count = 0
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


modifyState' :: (ServerState -> (ServerState, a)) -> Server a
modifyState' f = withState $ \stateVar -> do
    (state', a) <- readTVar stateVar >>= return . f
    writeTVar stateVar state'
    return a
    

modifyState :: (ServerState -> ServerState) -> Server ServerState
modifyState f = modifyState' (\state -> let state' = f state in (state', state'))


addClient :: Client -> ServerState -> ServerState
addClient client = over server_users (M.insert (client ^. client_id) client)

-- Remove a client:

removeClient :: UserId -> ServerState -> ServerState
removeClient client_id = over server_users (M.delete client_id)


broadcast :: (Binary a, Show a) =>  a -> Server ()
broadcast a = do
    liftIO $ print a
    users <- fmap (^. server_users) serverState
    
    for_ users $ \client -> 
      send (client ^. client_conn) a 

    
send :: (MonadIO m, Binary a, Show a) => WS.Connection -> a -> m ()
send conn msg = liftIO $ do
  putStrLn ("sending: " ++ show msg)
  WS.sendBinaryData conn str 
    where
      str = encode msg

eitherDecode :: Binary a => ByteString -> Either String a
eitherDecode b = case decodeOrFail b of
    Left (_, _, e)  -> Left e
    Right (_, _, x) -> Right x
      
receive :: (MonadIO m, Binary a, Show a) => WS.Connection -> m (Either String a)
receive conn = liftIO $ do
  str <- WS.receiveData conn
  let r = eitherDecode str
  putStrLn ("received: " ++ show r)
  return r
       
    
newId :: Server UserId
newId = modifyState' inc where
  inc state = (over server_count (+1) state, UserId $ state ^. server_count) 


sendClient :: (MonadIO m, Binary a, Show a) => Client -> a -> m ()
sendClient client a = send (client ^. client_conn) a

serverError :: (MonadIO m) => WS.Connection -> String -> m ()
serverError conn err = send conn (ServerError (T.pack err))
  
    
gameState ::  Client -> ServerState -> ServerMessage
gameState client state = Welcome (state^.server_game) users (client ^. client_id)  where 
   users = map _client_user $ M.elems (state ^. server_users) 
 
  
newUser :: Client -> Server ()
newUser  client = flip finally disconnect $ do      
  state <- modifyState $ addClient client
  
  sendClient client (gameState client state) 
  broadcast $ Connected i (client ^. client_user . user_name) 
  runClient client

  where   
    i = client ^. client_id
    disconnect = do
      modifyState $ removeClient i
      broadcast $ Disconnected i
    


runClient :: Client -> Server ()
runClient client = run' where
  run' = do
    received <- receive conn
    either onErr onCmd received
    
  onCmd msg = case msg of 
    ClientChat msg -> broadcast (Chat i msg) >> run'              
    
  onErr err = send conn (ServerError (T.pack err))

  i = client ^. client_id
  conn = client ^. client_conn
  
port :: Int
port = 9160

ip :: String
ip = "0.0.0.0"

application :: TVar ServerState -> WS.ServerApp
application stateVar pending = do
    
  print (WS.pendingRequest pending)  
    
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30

--   let loop i = do
--         WS.sendTextData conn (B.pack $ show i)
--         threadDelay 1000000  
--         loop (i + 1)
--   loop 0
  
  flip runReaderT stateVar $ do 
    i <- newId
    let user = User i (T.pack $ "noname" ++ show i)
    newUser  (Client user conn)
  


main :: IO ()
main = do
  stateVar <- newTVarIO newServerState
  
  putStrLn $ "squares-server listening on "++ ip ++ ":" ++ show port
  WS.runServer ip port $ application stateVar      
    
      