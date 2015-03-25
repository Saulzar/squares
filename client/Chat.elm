import Html (Html)

import List 
import Dict (Dict)

import Dict
import Maybe
import Maybe (..)
import Signal

import Debug
import String
  
import Util  

type alias UserName = String
  
type alias Users = Dict KeyCode UserName
type alias Chat {sender : UserName, message : String}

type alias Model = {id : Maybe Int, users :  Dict KeyCode Event : List Chat}

type Post = Post {message : String}

type Action 
  = Connect {id : Int, name : String}
    | Disconnect {id : Int}
    | Chat {sender : Int, message : String}
    | Error {reason : String}
  
  
update : Action -> Model -> Model
update action model = 
  case action of  
    Connect conn      -> model
    Disconnect discon -> model
    Chat chat         -> model
    Error err         -> model
    
 

       
       
initial : Model
initial = { model = Model.initial, selected = Nothing, keyMap = defaultKeys }


keyActions : Signal (Maybe Action)
keyActions = Signal.map (\k -> Just (KeyDown k)) Keyboard.lastPressed
                     
animateActions : Signal (Maybe Action)
animateActions = Signal.map (always (Just (Animate 1))) (Time.fps 50) 
                     
inputs : Signal (Maybe Action)
inputs = Signal.mergeMany  [
    keyActions, 
    Signal.subscribe updates,
    animateActions
  ]
                     
-- manage the model of our application over time
model : Signal Interface
model = Signal.foldp (maybe update) initial inputs
    
    
-- updates from user input
updates : Signal.Channel (Maybe Action)
updates = Signal.channel Nothing    
 
 
sendUpdate : Action -> Signal.Message
sendUpdate action = Signal.send updates (Just action)


view : Interface -> Html
view interface = 
  let squares = List.map (showSquare interface) (Dict.toList interface.model.squares)
  in svg [ version "1.1", x "0", y "0", viewBox "0 0 20 10" ] squares
  
    
main = Signal.map view model  


squareColour : Interface -> SquareId -> String
squareColour i s = let selectedCol = "skyblue"
                       defaultCol = "royalblue"
                   in if (i.selected == Just s) then selectedCol else defaultCol




catMaybes : List (Maybe a) -> List a
catMaybes = List.filterMap identity

showPivot : (Coord, Int) -> Html
showPivot (pivot, _) = circle ([fill "#000000", r "0.1"] ++ centre pivot) []

rotation : (Coord, Int) -> Attribute
rotation (pivot, deg) = rotate deg pivot


showSquare : Interface -> (SquareId, Square) -> Html
showSquare i (k, sq) = 
  let square = rectAt sq.pos 1 1
        ([ fill (squareColour i k),
           stroke "black",
           strokeWidth "0.02",
          onClick (sendUpdate (Select k))
        ] ++ catMaybes [rotation `Maybe.map` (Model.rotation sq)])
        
      pivot    = showPivot `Maybe.map` (Model.rotation sq)
  in
    g [] (catMaybes [Just square, pivot])
    
    
rectAt : Point a -> Int -> Int -> List Attribute -> Html
rectAt pos w h attrs = rect (width (toString w) :: height (toString h) :: position pos ++ attrs) []


position : Point a -> List Attribute
position p = [x (toString p.x), y (toString p.y)]
      
centre : Point a -> List Attribute
centre p = [cx (toString p.x), cy (toString p.y)]

rotate : b -> Point a -> Attribute
rotate deg centre = transform (String.concat ["rotate (", toString deg, ",", toString centre.x, ",", toString centre.y, ")"])
       
      