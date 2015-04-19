{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TupleSections, OverloadedLists, TemplateHaskell #-}


import Reflex
import Reflex.Dom


import qualified Data.Map as Map
import Data.Map (Map)


import Control.Lens

import Data.Monoid

import Control.Monad
import Control.Monad.IO.Class


import Game
import Dom


data Model = Model 
  { _model_game :: Game
  , _model_selected :: Maybe SquareId
  }
  

initial :: Model
initial = Model 
          { _model_game = initialGame
          , _model_selected = Just (SquareId 1) }
  
  
$( makeLenses ''Model)  

data Action = Select SquareId | Deselect deriving (Show)
  
  
showModel :: (MonadWidget t m) => Dynamic t Model -> m (Event t [Action])
showModel model = do
  
  squares <- mapDyn squareDisplay model
  svg_ attrs $ do 
    
    click <- clicked_
    let deselect = fmap (const [Deselect]) click
    
    selects <- listViewWithKey squares showSquare
    return $ leftmost {-mergeWith (++)-} [fmap Map.elems selects, deselect]
    
  where
    
    attrs = [ ("version", "1.1"),  ("viewBox", "0 0 20 10")
            , ("width", "100%"), ("preserveAspectRatio", "xMinYMin")] :: AttributeMap

      
      
squareDisplay :: Model -> Map SquareId (Coord, Bool)
squareDisplay model = Map.mapWithKey f squares where
  squares = model^.model_game.game_squares 
  f i sq  = (sq^.square_position, Just i == model^.model_selected) 
      

showSquare :: MonadWidget t m => SquareId -> Dynamic t (Coord, Bool) -> m (Event t Action)
showSquare i sq = do
  attrs <- mapDyn squareAttrs sq
  click <- rect_ attrs $ clicked_ 
  let select = fmap (const (Select i)) click

  return select
  
  
squareAttrs :: (Coord, Bool) -> AttributeMap
squareAttrs ((x, y), selected) = [("x", show x), ("y", show y)
          , ("width", "1"), ("height", "1"), ("fill", fill)] 
        where
          fill = if selected then "cadetblue" else "royalblue"
          



update :: Action -> Model -> Model  
update (Select i) = model_selected .~ (Just i)
update (Deselect) = model_selected .~ Nothing


updates :: [Action] -> Model -> Model
updates as m = foldr update m as 
  
  
  
  
main :: IO ()
main = mainWidget $ el "div" $ do
  rec 
    model <- foldDyn updates initial actions
    actions <- fmap (traceEvent "action") $ showModel model
    
  return ()
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