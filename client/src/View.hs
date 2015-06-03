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
  
gameEvent :: GameInput -> Model -> Maybe Action
gameEvent (ArrowKey dir) model = do
  selected <- _model_selected model
  fmap GameAction $ rotateDir (_model_game model) selected dir 
    

keyInput :: Model -> Int -> Maybe Action
keyInput model key = do
  input <- Map.lookup key (_model_keymap model)
  gameEvent input model  
  
  
              
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
    let deselect = fmap (const (Select Nothing)) click
    
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
  let select = fmap (const (Select $ Just i)) click

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
