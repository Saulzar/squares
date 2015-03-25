import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception.Lifted (finally)
import Control.Monad (forM_, forever)

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.Map as M
import Data.Foldable

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import Control.Concurrent.STM 
import Control.Lens

import Control.Arrow ((&&&))

import qualified Network.WebSockets as WS

import Data.Aeson
import GHC.Generics

import ClientMessage
import ServerMessage



data Client = Client
  { _clientId :: Int
  , _clientName :: Text
  , _clientConn :: WS.Connection
  }
  
instance Show Client where
  show (Client id name _) = show id ++ ": " ++ show name
  

data ServerState = ServerState 
  {  _serverUsers :: M.Map Int Client
  ,  _serverCount :: Int
  } deriving (Show)
  
 
$(makeLenses ''ServerState)
$(makeLenses ''Client)

-- Create a new, initial state:

newServerState :: ServerState
newServerState = ServerState {
    _serverUsers = M.empty,
    _serverCount = 0
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
    

modifyState :: (ServerState -> ServerState) -> Server ()
modifyState f = modifyState' (f &&& const ())



addClient :: Client -> ServerState -> ServerState
addClient client = over serverUsers (M.insert (client ^. clientId) client)

-- Remove a client:

removeClient :: Int -> ServerState -> ServerState
removeClient clientId = over serverUsers (M.delete clientId)


broadcast :: (ToJSON a, Show a) =>  a -> Server ()
broadcast a = do
    liftIO $ print a
    users <- fmap (^. serverUsers) serverState
    
    for_ users $ \client -> 
      sendJson (client ^. clientConn) a 

    
sendJson :: (MonadIO m, ToJSON a) => WS.Connection -> a -> m ()
sendJson conn err = liftIO $ WS.sendTextData conn (encode err)


recieveJson :: (MonadIO m, FromJSON a) => WS.Connection -> m (Either String a)
recieveJson conn = liftIO $ fmap eitherDecode' $ WS.receiveData conn
    
    
newId :: Server Int
newId = modifyState' inc where
  inc state = (over serverCount (+1) state, state ^. serverCount) 


  
loginUser :: Client -> Server ()
loginUser  client = flip finally disconnect $ do      
  modifyState $ addClient client
  broadcast $ Connected i (client ^. clientName) 
  runClient client

  where   
    i = client ^. clientId
    disconnect = do
      modifyState $ removeClient i
      broadcast $ Disconnected i
    


runClient :: Client -> Server ()
runClient client = run' where
  run' = do
    recieved <- recieveJson conn
    either onErr onCmd recieved
    
  onCmd msg = case msg of 
    ClientChat msg -> broadcast (Chat i msg) >> run'              
    
  onErr err = sendJson conn (ParseError (T.pack err))

  i = client ^. clientId
  conn = client ^. clientConn
  
      
        

application :: TVar ServerState -> WS.ServerApp
application stateVar pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    
    flip runReaderT stateVar $ do
      msg <- recieveJson conn
      i <- newId
      
      case msg of
        Right (Login name) -> loginUser  (Client i name conn)
        Left err           -> sendJson conn (T.pack err)


main :: IO ()
main = do
  stateVar <- newTVarIO newServerState
  WS.runServer "0.0.0.0" 9160 $ application stateVar      
    


        