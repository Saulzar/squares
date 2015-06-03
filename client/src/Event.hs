module Event where

import Reflex
import Reflex.Dom


import Control.Monad
import Control.Monad.IO.Class
import Control.Lens

import Control.Concurrent
import Data.Maybe



filterMaybes :: (Reflex t) => Event t (Maybe a) -> Event t a
filterMaybes = fmapMaybe id


splitMaybe :: (Reflex t) => Event t (Maybe a) -> (Event t (), Event t a)
splitMaybe e = (unTag $ ffilter isNothing e, filterMaybes e)



once :: (MonadWidget t m) => a -> m (Event t a) 
once a = do
  postBuild <- getPostBuild    
  return (fmap (const a) postBuild) 
  

tagConst :: (Reflex t) => a -> Event t b -> Event t a
tagConst a = fmap (const a) 


unTag :: (Reflex t) => Event t a -> Event t ()
unTag = fmap (const ())

holdJust :: (MonadWidget t m) => Event t a -> m (Behavior t (Maybe a))
holdJust e = hold Nothing (fmap Just e)

holdJustDyn :: (MonadWidget t m) => Event t a -> m (Dynamic t (Maybe a))
holdJustDyn e = hold Nothing (fmap Just e)
  
splitWhen :: (Reflex t) => (a -> Maybe b) -> Event t a -> (Event t b, Event t a)
splitWhen sel event = (fmapMaybe sel event, ffilter (isNothing . sel) event)
  
  
bufferEvents :: (MonadWidget t m) => Event t a -> Event t b -> m (Dynamic t [a])
bufferEvents add clear = foldDyn ($) [] $ leftmost 
      [ fmap (const $ const []) clear --Clear buffer
      , fmap (:) add 
      ]
      
      
keepLast :: (MonadWidget t m) => Event t a -> Event t b -> m (Dynamic t (Maybe a))
keepLast setter clear = holdDyn Nothing $ leftmost 
      [ fmap (const Nothing) clear 
      , fmap Just setter
      ]  

-- Wrapper for performEventAsync where the function is called in another thread
forkEventAsync :: MonadWidget t m => Event t a -> (a -> (b -> IO ()) -> IO ()) -> m (Event t b)
forkEventAsync e f = performEventAsync $ ffor e $ \a cb -> liftIO $ do 
    void $ forkIO $ f a cb
    

    
performAsync ::  MonadWidget t m => Event t a -> (a -> IO b) -> m (Event t b)
performAsync e f = forkEventAsync e (\a cb -> f a >>= cb) 


-- Filter event stream with predicate and return ()
whenE_ :: (Reflex t) => (a -> Bool) -> Event t a -> Event t () 
whenE_ f = fmap (const ()) . ffilter f 

-- Split Either into two event streams
splitEither :: (Reflex t) => Event t (Either a b) -> (Event t a, Event t b)
splitEither e = (fmapMaybe (firstOf _Left) e, fmapMaybe (firstOf _Right) e)


-- Attach an event with it's previous value
history :: MonadWidget t m => Event t a -> m (Event t (Maybe a, a))
history event = do 
  val <- hold Nothing (fmap Just event)
  return $ attach val event
  

counter :: MonadWidget t m => Event t a -> m (Dynamic t Int)
counter event = foldDyn (+) 0 (fmap (const 1) event)


  
delay :: MonadWidget t m => Int -> Event t a -> m (Event t a)
delay n event = performAsync event (\a -> threadDelay n >> return a)