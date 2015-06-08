{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TupleSections, OverloadedLists #-}
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
import Event

import Game.View
import Game.Types
       



    
singlePlayer :: forall t m. (MonadWidget t m) => GameModel -> m ()
singlePlayer initial = do
  window <- askWindow 
  animate <- animationEvent window      
  
  rec 
    model  <- foldMany updateGame initial actions         
    inputs <- gameView  model settings

    let settings = constant defaultSettings
        actions = mconcat 
          [ inputs
          , tagConst [Animate 4] animate
          ]
 
        
  return ()
  

  
  
main :: IO ()
main = mainWidgetWithCss $(embedFile "style.css") $ el "div" $ (singlePlayer initial) where
    initial = initialModel initialGame
  

