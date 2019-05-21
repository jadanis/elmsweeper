module Grid exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode

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

empty : Grid a
empty = 
    []

decode : Decode.Decoder a -> Decode.Decoder (Grid a)
decode cell =
    Decode.list
        (Decode.map6
            Cell
            (Decode.field "val" cell)
            (Decode.field "pos" (Decode.map2 (\a b -> (a,b)) (Decode.index 0 Decode.int) (Decode.index 1 Decode.int)))
            (Decode.field "mine" Decode.bool)
            (Decode.field "flag" Decode.bool)
            (Decode.field "neigh" Decode.int)
            (Decode.field "rev" Decode.bool)
        )


encode : (a -> Encode.Value) -> Grid a -> Encode.Value
encode cell grid =
    let
        encodeCell { val, pos, mine, flag, neigh, rev } =
            Encode.object
                [ ("val", cell val)
                , ("pos"
                  , Encode.list Encode.int
                        [ Tuple.first pos
                        , Tuple.second pos
                        ]
                  )
                , ("mine",Encode.bool mine)
                , ("flag", Encode.bool flag)
                , ("neigh", Encode.int neigh)
                , ("rev", Encode.bool rev)
                ]
    in
        Encode.list encodeCell grid
