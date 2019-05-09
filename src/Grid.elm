module Grid exposing (..)


type alias Cell a =
    { val : a
    , pos : (Int,Int)
    , mine : Bool
    , flag : Bool
    , neigh : Int
    , rev : Bool
    }


blankCell : a -> Cell a
blankCell a =
    { val = a
    , pos = (0,0)
    , mine = False
    , flag = False
    , neigh = 0
    , rev = False
    }


type alias Grid a =
    List (Cell a)


mapToList : (a -> (Int, Int) -> b) -> Grid a -> List b
mapToList fun =
    List.map (\{val, pos} -> fun val pos)


empty : Grid a
empty = 
    []
