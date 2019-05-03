module Grid exposing (Grid)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias Cell a =
    { val : a
    , pos : ( Int, Int )
    , mine : Bool
    , flag : Bool
    , neigh : Int
    }

blankCell : a -> Cell a
blankCell a = 
    { val = a 
    , pos = (0,0)
    , mine = False
    , flag = False
    , neigh = 0
    }

type alias Grid a =
    List (Cell a)


fromList : List (a,Bool,Bool,Int) -> Grid a
fromList {x, y, z, w} =
    let
        i = 0
        j = 0
    in
    Cell x (i,j) y z w


mapToList : (a -> ( Int, Int ) -> b) -> Grid a -> List b
mapToList fun =
    List.map (\{ val, pos } -> fun val pos)


empty : Grid a
empty =
    []

-- stamps a grid into another grid with predefined offset


stamp : Int -> Int -> Grid a -> Grid a -> Grid a
stamp x y sample grid =
    case sample of
        [] ->
            grid

        cell :: rest ->
            let
                newPos =
                    ( Tuple.first cell.pos + x, Tuple.second cell.pos + y )

                newCell =
                    { cell | pos = newPos }
            in
            stamp x y rest ({ cell | pos = newPos } :: List.filter (\{ pos } -> pos /= newPos) grid)


size : Grid a -> ( Int, Int )
size grid =
    let
        ( x, y ) =
            List.unzip (List.map .pos grid)

        dimension d =
            Maybe.withDefault 0 (List.maximum (List.map (\a -> a + 1) d))
    in
    ( dimension x, dimension y )


decode : Decoder a -> Decoder (Grid a)
decode cell =
    Decode.list
        (Decode.map2
            Cell
            (Decode.field "val" cell)
            (Decode.field "pos" (Decode.map2 (\a b -> ( a, b )) (Decode.index 0 Decode.int) (Decode.index 1 Decode.int)))
        )


encode : (a -> Value) -> Grid a -> Value
encode cell grid =
    let
        encodeCell { val, pos } =
            Encode.object
                [ ( "pos"
                  , Encode.list Encode.int
                        [ Tuple.first pos
                        , Tuple.second pos
                        ]
                  )
                , ( "val", cell val )
                ]
    in
    Encode.list encodeCell grid
