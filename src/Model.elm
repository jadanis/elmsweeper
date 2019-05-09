module Model exposing (..)

import Random
import Grid exposing (..)
import Color exposing (..)
import Dict


type State
    = Playing
    | Lost
    | Won


type alias Model =
    { status : State
    , grid : Grid Color
    , seed : Random.Seed
    }


newGame : Random.Seed -> Model
newGame seed =
    let
        (grid, nseed) =
            random seed 10 10
    in
        { status = Playing
        , grid = grid
        , seed = nseed 
        }
    

init : Model
init = newGame (Random.initialSeed 0)

random : Random.Seed -> Int -> Int -> (Grid Color, Random.Seed)
random seed height width =
    let
        mines = 
            Random.list (height * width) (Random.int 0 5)
        
        cell n =
            let
                ncell = blankCell covered
                mine = (n == 0)
            in
                { ncell | mine = mine }
        field nums = correct height width (List.map cell nums)
    in
        Random.step (Random.map field mines) seed


correct : Int -> Int -> Grid a -> Grid a
correct h w grid =
    let
        gridIdx = List.indexedMap Tuple.pair grid
        gridp =
            let
                f (t,cell) =
                    {cell | pos = (30 * (t//w), 30 * (modBy w t))}
            in
                List.map f gridIdx
        dict = Dict.fromList (List.map (\cell -> (cell.pos,cell)) gridp)
        getNeigh d key =
            case (Dict.get key d) of
                Nothing ->
                    0
                Just c ->
                    if c.mine then
                        1
                    else
                        0
        cellsn (a,b) =
            [(a-30,b-30),(a-30,b),(a-30,b+30),(a,b-30),(a,b),(a,b+30),(a+30,b-30),(a+30,b),(a+30,b+30)]
        neighbors (x,y) =
            List.sum (List.map (getNeigh dict) (cellsn (x,y)))
        ccell cell =
            {cell | neigh = (neighbors cell.pos)}
    in
        List.map ccell gridp
