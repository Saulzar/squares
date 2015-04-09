module Util where


bool : Bool -> a -> Maybe a
bool b x = if b then Just x else Nothing

       
concatMaybes : Maybe a -> Maybe a -> Maybe a
concatMaybes a b = case a of 
    Just a -> Just a
    _      -> b       
    

catMaybes : List (Maybe a) -> List a
catMaybes = List.filterMap identity
    
    
isJust : Maybe a -> Bool
isJust a = case a of 
  Just a  -> True
  Nothing -> False
     
     
maybe : b -> (a -> b) -> Maybe a -> b    
maybe def f ma = case ma of
  Just a  -> f a
  Nothing -> def   
     
mapSnd : (a -> b) -> (c, a) -> (c, b) 
mapSnd f (c, a) = (c, f a)     


mapFst : (a -> b) -> (c, a) -> (c, b) 
mapFst f (c, a) = (c, f a)