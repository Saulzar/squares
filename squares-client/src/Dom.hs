{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}

module Dom 
  ( clicked_, keypress_, keydown_, scrolled_
  
  , windowKeydown_
  , htmlElement, svgElement
  
  , svg_, rect_, g_
  , div_
  
  , getBody
  , requestAnimationFrame
  , askWindow
  
  ) where

import Reflex
import Reflex.Dom  

import GHCJS.DOM.Types hiding (Event)
import GHCJS.DOM.Document
import GHCJS.DOM.DOMWindow
  
import GHCJS.DOM.HTMLElement
import GHCJS.DOM.Element

import GHCJS.DOM.UIEvent
import GHCJS.DOM.EventM (event, EventM, stopImmediatePropagation)
import GHCJS.DOM.Node (nodeAppendChild, toNode)  

import Data.Dependent.Sum (DSum ((:=>)))

import Control.Monad
import Control.Monad.IO.Class

import Reflex.Dom.Class
import Reflex.Host.Class

import GHCJS.Types
import GHCJS.Foreign


type RequestAnimationFrameCallback =  IO ()


foreign import javascript unsafe
        "$1[\"requestAnimationFrame\"]($2)" js_requestAnimationFrame ::
        JSRef DOMWindow ->  JSFun RequestAnimationFrameCallback -> IO Int

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.requestAnimationFrame Mozilla Window.requestAnimationFrame documentation> 
requestAnimationFrame ::
                      (MonadIO m) =>
                        DOMWindow -> RequestAnimationFrameCallback -> m Int
requestAnimationFrame self callback
  = liftIO $ do
      cb <- asyncCallback AlwaysRetain callback
      (js_requestAnimationFrame (unDOMWindow self) cb )
 
foreign import javascript unsafe "$1[\"cancelAnimationFrame\"]($2)"
        js_cancelAnimationFrame :: JSRef DOMWindow -> Int -> IO ()

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window.cancelAnimationFrame Mozilla Window.cancelAnimationFrame documentation> 
cancelAnimationFrame :: (MonadIO m) => DOMWindow -> Int -> m ()
cancelAnimationFrame self id
  = liftIO (js_cancelAnimationFrame (unDOMWindow self) id)
 
 



wrapDomEvent' onEvent getResult e = wrapDomEvent e onEvent getResult  

askParent' :: (MonadWidget t m) => m Element
askParent' = fmap castToElement askParent
  
clicked_ :: (MonadWidget t m) => m (Event t ())
clicked_ = askParent' >>= wrapDomEvent' elementOnclick (stopImmediatePropagation >> return ())

keypress_ :: (MonadWidget t m) => m (Event t Int)
keypress_ = askParent' >>= wrapDomEvent' elementOnkeypress  (liftIO . uiEventGetKeyCode =<< event)


askWindow :: (MonadIO m, HasDocument m) => m DOMWindow
askWindow =  do 
  (Just window) <- askDocument >>= liftIO . documentGetDefaultView 
  return window


windowKeydown_ :: (MonadWidget t m) => m (Event t Int)
windowKeydown_ = askWindow >>= wrapDomEvent' domWindowOnkeydown  (liftIO . uiEventGetKeyCode =<< event)


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

  
  