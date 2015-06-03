{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TupleSections, OverloadedLists #-}


module View where

import Reflex
import Reflex.Dom

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Data.List (intersperse)

import Control.Lens
import Data.Monoid 

import Control.Monad 
import Control.Monad.IO.Class

import Squares.Game
import Squares.Types

import Data.Text (Text)

import Dom
import Types
import Event
  


defaultKeys :: Map Int GameInput
defaultKeys =
  [ (37, ArrowKey LeftDir)
  , (38, ArrowKey UpDir)
  , (39, ArrowKey RightDir)
  , (40, ArrowKey DownDir)
  ]         
  
  
initial :: PlayingState
initial = PlayingState 
  { _model_game = emptyGame
  , _model_user = UserId 0
  , _model_selected = Nothing
  , _model_keymap = defaultKeys
  }
  
  
gameMove :: GameInput -> Model -> Maybe Action
gameMove (ArrowKey dir) model = do
  selected <- _model_selected model
  fmap MoveAction $ rotateDir (_model_game model) selected dir 
    

keyInput :: Model -> Int -> Maybe Action
keyInput model key = do
  input <- Map.lookup key (_model_keymap model)
  gameMove input model  
  
  
              
listView :: (MonadWidget t m, Ord k) => Dynamic t (Map k v) -> (k -> Dynamic t v -> m (Event t b)) -> m (Event t b)
listView xs view = do 
  events <- listViewWithKey xs view
  return $ fmapMaybe (listToMaybe . Map.elems) events
  
  
maybeToMap :: k -> Maybe a -> Map k v
maybeToMap k Nothing = Map.empty
maybeToMap k (Just a) = Map.singleton k a
  
maybeView :: Dynamic t (Maybe a) ->  (Dynamic t a -> m (Event t b)) -> m (Event t b)
maybeView a view = do
    list <- mapDyn (maybeToMap 1) a
    listView list (const view)
  
 

showModel :: (MonadWidget t m) => Dynamic t Model -> m (Event t Action)
showModel model = do
    playing <- mapDyn (^? _Playing) model)  
    maybeView playing showPlaying
    
    login <- mapDyn (^? _Login) model)  
    maybeView playing showPlaying
    
    
 

showLogin :: (MonadWidget t m) => m (Event t Action)
showLogin = return never

  
showPlaying :: (MonadWidget t m) => Dynamic t Playing -> m (Event t Action)
showPlaying model = do
  
  squares <- mapDyn squareDisplay model
  svg_ attrs $ do 
    
    allKeys <- windowKeydown_  
    let keyEvt =  attachWithMaybe keyInput (current model) allKeys
  
    clickEvt <- clicked_ 
    let deselectEvt = fmap (const (Select Nothing)) clickEvt
    
    selectEvt <-  listView squares showSquare
    return $ leftmost [deselectEvt, selectEvt, keyEvt]
    
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
  let selectEvt = fmap (const (Select $ Just i)) click

  return selectEvt
  
  
squareAttrs :: SquareApp -> AttributeMap
squareAttrs (V2 x y, selected, r) = [("x", show x), ("y", show y)
          , ("width", "1"), ("height", "1"), ("fill", fill)] <> (maybe [] rotateAround r) 
        where
          fill = if selected then "cadetblue" else "royalblue"
          
commas :: Show a => [a] -> String
commas = concat . intersperse ", " . map show
          
          
rotateAround :: (Coord, Int) -> AttributeMap
rotateAround (V2 cx cy, angle) = [("transform",  "rotate (" ++ commas [angle, cx, cy] ++ ")")]
