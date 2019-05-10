module Model exposing (..)

import Random
import Grid exposing (..)
import Color exposing (..)
import Dict
import Time
import Json.Decode as Decode
import Json.Encode as Encode


type State
    = Playing
    | Lost
    | Won


type alias Model =
    { status : State
    , grid : Grid Color
    , seed : Random.Seed
    , games : Int
    , wins : Int
    , start : Time.Posix
    , curr : Time.Posix
    }


newGame : Random.Seed -> (Grid Color, Random.Seed)
newGame seed = random seed 10 10
    

init : Model
init = 
    let
        (grid, seed) =
            newGame (Random.initialSeed 0)
    in
        { status = Playing
        , grid = grid 
        , seed = seed
        , games = 0
        , wins = 0
        , start = Time.millisToPosix 0
        , curr = Time.millisToPosix 0
        }

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


decode : Decode.Decoder Model
decode =
    Decode.map6
        (\status grid games wins start curr ->
            { init
                | status = status
                , grid = grid  
                --, seed = seed
                , games = games
                , wins = wins
                , start = start
                , curr = curr
            }    
        )
        (Decode.field "status" (Decode.map decodeState Decode.string))
        (Decode.field "grid" (Grid.decode Color.decode))
        --(Decode.field "seed" (Decode.map decodeSeed (Decode.list Decode.int)))
        (Decode.field "games" Decode.int)
        (Decode.field "wins" Decode.int)
        (Decode.field "start" (Decode.map decodeTime Decode.string))
        (Decode.field "curr" (Decode.map decodeTime Decode.string))


encode : Int -> Model -> String
encode indent model =
    Encode.encode
        indent
        (Encode.object
            [ ("status", Encode.string <| encodeState model.status)
            , ("grid", Grid.encode Color.encode model.grid)
            --, ("seed", seedEncode model.seed) -- (String, Encode.Value)
            , ("games", Encode.int model.games)
            , ("wins", Encode.int model.wins)
            , ("start", Encode.int <| Time.posixToMillis model.start)
            , ("curr", Encode.int <| Time.posixToMillis model.curr)
            ]
        )


decodeState : String -> State
decodeState string =
    case string of
        "lost" ->
            Lost
        "won" ->
            Won
        _ ->
            Playing



encodeState : State -> String
encodeState state =
    case state of
        Playing ->
            "playing"
        Won ->
            "won"
        Lost ->
            "lost"


decodeTime : String -> Time.Posix
decodeTime string = Time.millisToPosix <| Maybe.withDefault 0 <| String.toInt string


{-decodeSeed : List Int -> Random.Seed
decodeSeed (x :: y :: xs) =
    Random.Seed x y

encodeSeed : Random.Seed -> Encode.Value
encodeSeed (Random.Seed n0 n1) =
    Encode.list Encode.int [n0,n1]-}

