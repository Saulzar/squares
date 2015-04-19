module Dom 
  ( clicked_, keypress_, keydown_, scrolled_
  , htmlElement, svgElement
  
  , svg_, rect_, g_
  , div_
  
  , getBody
  
  ) where

import Reflex
import Reflex.Dom  

  
import GHCJS.DOM.Document
import GHCJS.DOM.HTMLElement
import GHCJS.DOM.Element

import GHCJS.DOM.UIEvent
import GHCJS.DOM.EventM (event, EventM, stopImmediatePropagation)
import GHCJS.DOM.Node (nodeAppendChild, toNode)  
  
import Control.Monad.IO.Class


wrapDomEvent' onEvent getResult e = wrapDomEvent e onEvent getResult  

askParent' :: (MonadWidget t m) => m Element
askParent' = fmap castToElement askParent
  
clicked_ :: (MonadWidget t m) => m (Event t ())
clicked_ = askParent' >>= wrapDomEvent' elementOnclick (stopImmediatePropagation >> return ())

keypress_ :: (MonadWidget t m) => m (Event t Int)
keypress_ = askParent' >>= wrapDomEvent' elementOnkeypress  (liftIO . uiEventGetKeyCode =<< event)

keydown_ :: (MonadWidget t m) => m (Event t Int)
keydown_ = askParent' >>= wrapDomEvent' elementOnkeydown  (liftIO . uiEventGetKeyCode =<< event)

scrolled_ :: (MonadWidget t m) => m (Event t Int)
scrolled_ = askParent' >>= \e -> wrapDomEvent e elementOnscroll $ liftIO $ elementGetScrollTop e
  
htmlElement :: (MonadWidget t m, Attributes m attrs) => String -> attrs -> m a -> m a
htmlElement elementTag attrs child = do
  e <- buildEmptyElement elementTag attrs
  subWidget (toNode e) child
  
div_ :: (MonadWidget t m, MonadIO m, Attributes m attrs) => attrs -> m a -> m a
div_ = htmlElement "div"  
  
  
svgNamespace :: String
svgNamespace = "http://www.w3.org/2000/svg"  
  
svgElement :: (MonadWidget t m, Attributes m attrs) => String -> attrs -> m a -> m a
svgElement elementTag attrs child = do
  e <- buildEmptyElementNS (Just svgNamespace) elementTag attrs
  subWidget (toNode e) child   
  

svg_, rect_, g_ :: (MonadWidget t m, MonadIO m, Attributes m attrs) => attrs -> m a -> m a
svg_ = svgElement "svg"
rect_ = svgElement "rect"
g_ = svgElement "g"

getBody :: (MonadWidget t m, MonadIO m) => m (El t)
getBody = do
  doc <- askDocument
  Just b <- liftIO $ documentGetBody doc
  wrapElement (castToElement b)

  
  