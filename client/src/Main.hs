{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TupleSections, OverloadedLists, TemplateHaskell #-}
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
import WebSocket


data Model = Model 
  { _model_game :: Game
  , _model_selected :: Maybe SquareId
  , _model_keymap :: Map Int GameInput
  , _model_connected :: Bool
  }
  
  
defaultKeys :: Map Int GameInput
defaultKeys = 
  [ (37, ArrowKey LeftDir)
  , (38, ArrowKey UpDir)
  , (39, ArrowKey RightDir)
  , (40, ArrowKey DownDir)
  ]  

initial :: Model
initial = Model 
          { _model_game = initialGame
          , _model_selected = Nothing
          , _model_keymap = defaultKeys
          }
  
  
$( makeLenses ''Model)  


data Action = Select SquareId 
            | Deselect 
            | GameAction GameEvent 
            | Animate Int 
            | MadeConnection  
              deriving (Show)
  
listView :: (MonadWidget t m, Ord k) => Dynamic t (Map k v) -> (k -> Dynamic t v -> m (Event t b)) -> m (Event t b)
listView xs view = do 
  events <- listViewWithKey xs view
  return $ fmapMaybe (listToMaybe . Map.elems) events
  
showModel :: (MonadWidget t m) => Dynamic t Model -> m (Event t Action)
showModel model = do
  
  squares <- mapDyn squareDisplay model
  svg_ attrs $ do 
    
    allKeys <- windowKeydown_  
    let key =  attachWithMaybe keyInput (current model) allKeys
  
    click <- clicked_
    let deselect = fmap (const Deselect) click
    
    select <-  listView squares showSquare
    return $ leftmost [deselect, select, key]
    
  where
    
    attrs = [ ("version", "1.1"),  ("viewBox", "0 0 20 10")
            , ("width", "100%"), ("preserveAspectRatio", "xMinYMin")] :: AttributeMap


type SquareApp = (Coord, Bool, Maybe (Coord, Int))

            
      
squareDisplay :: Model -> Map SquareId SquareApp
squareDisplay model = Map.mapWithKey f squares where
  squares = model^.model_game . game_squares 
  f i sq  = (sq^.square_position, Just i == model^.model_selected, progress i (model^.model_game)) 
      

showSquare :: MonadWidget t m => SquareId -> Dynamic t SquareApp -> m (Event t Action)
showSquare i sq = do
  attrs <- mapDyn squareAttrs sq
  click <- rect_ attrs $ clicked_ 
  let select = fmap (const (Select i)) click

  return select
  
  
squareAttrs :: SquareApp -> AttributeMap
squareAttrs (V2 x y, selected, r) = [("x", show x), ("y", show y)
          , ("width", "1"), ("height", "1"), ("fill", fill)] <> (maybe [] rotateAround r) 
        where
          fill = if selected then "cadetblue" else "royalblue"
          
commas :: Show a => [a] -> String
commas = concat . intersperse ", " . map show
          
          
rotateAround :: (Coord, Int) -> AttributeMap
rotateAround (V2 cx cy, angle) = [("transform",  "rotate (" ++ commas [angle, cx, cy] ++ ")")]
       

          



update :: Action -> Model -> Model  
update (Select i) = model_selected .~ (Just i)
update (Deselect) = model_selected .~ Nothing
update (GameAction e) = model_game %~ runEvent e 
update (Animate dt) = model_game %~ animate dt
update (MadeConnection) = model_connected .~ True


      
    
gameEvent :: GameInput -> Model -> Maybe Action
gameEvent (ArrowKey dir) model = do
  selected <- _model_selected model
  fmap GameAction $ rotateDir (_model_game model) selected dir 
    

keyInput :: Model -> Int -> Maybe Action
keyInput model key = do
  input <- Map.lookup key (_model_keymap model)
  gameEvent input model  
  
  
  
serverInput :: Model -> ServerMessage -> Maybe Action
serverInput model msg = Nothing


  
remote :: (MonadWidget t m) => Event t ClientMessage -> m (Event t ServerMessage)
remote outgoing = do
  rec
    
    socket <- receiveData connection 
    performEvent_ $ ffor (socket_decodeFail socket) $ \msg -> 
      error $ "failed to decode message: " ++ show msg
      
    needConnect <- whenDyn (isNothing) connection
    connection <- holdConnection $ fmap (const url) needConnect

  return (socket_message socket)

  where
    url = "ws://0.0.0.0:9160" :: Text  

  
    
once :: (MonadWidget t m) => a -> m (Event t a) 
once a = do
  postBuild <- getPostBuild    
  return (fmap (const a) postBuild) 



whenDyn :: (MonadWidget t m) => (a -> Bool) -> Dynamic t a -> m (Event t a)
whenDyn f dyn = do
  postBuild <- getPostBuild    
  return $ ffilter f (tagDyn dyn postBuild)
  
  
    
showWindow :: forall t m. (MonadWidget t m) =>  m ()
showWindow = do
  window <- askWindow 
  animate <- animationEvent window      
  
  rec 
    let outgoing = never :: Event t ClientMessage 
    

    incoming <- remote outgoing
    model    <- foldDyn update initial actions
    
    
    inputs <-  showModel model
    let actions = {-traceEvent "action" $-} leftmost 
          [ inputs
          , attachWithMaybe serverInput (current model) incoming
          , fmap (const $ Animate 4) animate
          ]     
        
  return ()
  
  
  
main :: IO ()
main = mainWidgetWithCss $(embedFile "style.css") $ el "div" $ showWindow
  

  
  
--     mergeMap clicks 
    
    
    
--   text "Laaaalahh"
--   t <- textInput
--   body <- getBody
--     
--   text "Last key pressed: "
--   let keypressEvent = fmap show $ _el_keypress body
--   keypressDyn <- holdDyn "None" keypressEvent
--   dynText keypressDyn    
--     
--   return ()  