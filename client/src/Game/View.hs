{-# LANGUAGE OverloadedLists #-}

module Game.View (gameView, updateGame, defaultSettings, gameModel) where


import Reflex
import Reflex.Dom

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Data.List (intersperse)

import Control.Lens
import Data.Monoid 

import Control.Monad.IO.Class


import Squares.Game
import Squares.Types

import Data.Text (Text)

import Dom
import Event

import Game.Types


updateGame :: GameAction -> GameModel -> GameModel  
updateGame (Select i)        = gm_selected .~ i
updateGame (MoveAction e)    = gm_game %~ runMove e 
updateGame (Animate dt)      = gm_game %~ animate dt
updateGame (EventAction e)   = gm_game %~ runEvent e


gameModel :: Game -> GameModel
gameModel game = GameModel 
  { _gm_game = game
  , _gm_user = UserId 0
  , _gm_selected = Nothing
  } 


defaultKeys :: Map Int GameInput
defaultKeys =
  [ (37, ArrowKey LeftDir)
  , (38, ArrowKey UpDir)
  , (39, ArrowKey RightDir)
  , (40, ArrowKey DownDir)
  ]         
  
  
defaultSettings ::  Settings
defaultSettings = Settings {
    _settings_keyMap = defaultKeys
  }
  
  
gameAction ::  GameModel -> GameInput -> Maybe GameAction
gameAction model (ArrowKey dir)  = do
  selected <- model ^. gm_selected 
  fmap (MoveAction . (model^.gm_user,) ) $ rotateDir (model^.gm_game) selected dir 
    

    
  
gameView :: (MonadWidget t m) => Dynamic t GameModel -> Behavior t Settings -> m (Event t [GameAction])
gameView model settings = do
  
  svg_ attrs $ do 
    
    keyPress <- windowKeydown_  
    let keyInput  = attachWithMaybe (flip Map.lookup) keyMap keyPress 
        keyAction = attachWithMaybe gameAction (current model) keyInput
  
    click <- clicked_ 
    let deSelect = fmap (const (Select Nothing)) click
    
    
    squares <- mapDyn squareDisplay model
    select <-  listView squares squareView
    return $ mconcat $ map toList [deSelect, select, keyAction]
    
  where
    
    keyMap = fmap (^.settings_keyMap) settings
    toList = fmap (:[])
    attrs = [ ("version", "1.1"),  ("viewBox", "0 0 20 10")
            , ("width", "100%"), ("preserveAspectRatio", "xMinYMin")] :: AttributeMap


type SquareApp = (Coord, Bool, Maybe (Coord, Int))

            
      
squareDisplay :: GameModel -> Map SquareId SquareApp
squareDisplay model = Map.mapWithKey f squares where
  squares = model^.gm_game . game_squares 
  f i sq  = (sq^.square_position, Just i == model^.gm_selected, progress i (model^.gm_game)) 
      

squareView :: MonadWidget t m => SquareId -> Dynamic t SquareApp -> m (Event t GameAction)
squareView i sq = do
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
  