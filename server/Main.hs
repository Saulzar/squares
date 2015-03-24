import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.Map as M
import Data.Traversable

import Control.Monad.Trans.Reader

import Control.Concurrent.STM 
import Control.Lens

import Control.Arrow ((&&&))

import qualified Network.WebSockets as WS

import Data.Aeson
import GHC.Generics


data Login = Login { _loginName :: !Text } deriving (Show, Generic)

instance FromJSON Login
instance ToJSON Login


data ClientErr = LoginError { _errReason :: !Text } deriving (Show, Generic)

instance FromJSON ClientErr
instance ToJSON ClientErr


type Client = (Text, WS.Connection)
data ServerState = ServerState 
  {  _serverUsers :: M.Map Int Client
  ,  _serverCount :: Int
  }
  
 
$(makeLenses ''ServerState)

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
modifyState f = modifyState' (f &&& id)


main = undefined

{-


addClient :: Client -> ServerState -> ServerState
addClient client state = state { serverUsers =  clients } where
  clients = M.insert (client ^. clientId) client (serverUsers state)

-- Remove a client:

removeClient :: Int -> Server ()
removeClient clientId = state { serverUsers = M.delete clientId (serverUsers state) }


    

broadcast :: (ToJSON a, Show a) =>  a -> Server ()
broadcast a = do
    print a
    users <- fmap serverUsers serverState
    
    forM_ users $ \(_, conn) -> 
      sendJson conn a 

    
sendJson :: (MonadIO m, ToJSON a) => WS.Connection -> a -> IO ()
sendJson conn err = liftIO $ WS.sendTextData conn (encode err)


recieveJson :: (MonadIO m, FromJSON a) => WS.Connection -> IO (Maybe a)
recieveJson = liftIO $ WS.receiveData conn >>= fmap decode    
    
    
newId :: Server Int
newId = modifyState' inc where
  inc state = (over serverCount (+1) state, state ^. serverCount) 

loginUser :: (Text, WS.Connection) -> Server ()
loginUser  client = flip finally disconnect $ do
  
  id <- newId
  modifyState $ addClient (Client id user conn)
  
  broadcast  (Connected (client ^. clientName) (client ^. client )
  
  
  where
    disconnect = do
      modifyState $ removeClient (user, conn)
      broadcast stateVar (Disconnected user)
      
      

runClient :: TVar ServerState -> Client -> IO ()
runClient stateVar (user, conn) = forever $ do
    msg <- WS.recieveJson conn
    
    case msg of 
      UserMessage msg -> broadcast  (Message user msg)
        
      

application :: TVar ServerState -> WS.ServerApp
application pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30

    msg <- recieveJson
    case msg of
      Just (Login user) -> loginUser  (user, conn)
      Nothing           -> sendError conn (LoginError "could not parse login")


        
        
-- The main function first creates a new state for the server, then spawns the
-- actual server. For this purpose, we use the simple server provided by
-- `WS.runServer`.

main :: IO ()
main = do
    stateVar <- newTVar newServerState
    WS.runServer "0.0.0.0" 9160 $ application state        -}
        
        