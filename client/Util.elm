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
     