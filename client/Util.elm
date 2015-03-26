module Util where

import Maybe
import List

bool : Bool -> a -> Maybe a
bool b x = if b then Just x else Nothing

maybe : (a -> b -> b) -> Maybe a -> b -> b
maybe f ma b = 
  case ma of
       Nothing -> b
       Just a  -> f a b
       
       
concatMaybes : Maybe a -> Maybe a -> Maybe a
concatMaybes a b = case a of 
    Just a -> Just a
    _      -> b       
    

catMaybes : List (Maybe a) -> List a
catMaybes = List.filterMap identity
    