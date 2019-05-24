module Model exposing (State(..),Model,init,encode,decode,newGame)

import Random exposing (initialSeed,step,list)
import Grid exposing (Grid, encode, decode,blankCell)
import Color exposing (Color,covered,encode,decode)
import Time exposing(Posix, millisToPosix, posixToMillis)
import Json.Decode as Decode
import Json.Encode as Encode
import Set exposing (fromList, toList)


type State
    = Playing
    | Lost
    | Won
    | Paused
    | New


type alias Model =
    { status : State
    , grid : Grid Color
    , seed : Random.Seed
    , games : Int
    , wins : Int
    , start : Time.Posix
    , curr : Time.Posix
    , flags : Int
    }

init : Model
init = 
    let
        (grid, seed) =
            newGame (Random.initialSeed 0)

        model = 
            { status = New
            , grid = grid 
            , seed = seed
            , games = 0
            , wins = 0
            , start = Time.millisToPosix 0
            , curr = Time.millisToPosix 0
            , flags = 0
            }
    in
        model


newGame : Random.Seed -> (Grid Color, Random.Seed)
newGame seed = random seed 10 10


random : Random.Seed -> Int -> Int -> (Grid Color, Random.Seed)
random seed height width =
    let
        num_mines = (height * width) // 5

        mineGenerator = randomCoor height width

        (m,nseed) =
            setStep num_mines seed mineGenerator

        mines = 
            List.map (\(x,y) -> (x*30,y*30)) m

        cell n = blankCell covered

        field nums = correct height width (List.map cell nums)

        grid = List.map (\c -> if List.member c.pos mines then {c | mine = True} else c) (field <| List.range 0 <| (height * width) - 1)
    in
        (grid, nseed)


setStep : Int -> Random.Seed -> Random.Generator comparable -> (List comparable, Random.Seed)
setStep n seed gen =
    let
        listgen = Random.list n gen
        (a,nseed) = Random.step listgen seed
        s = Set.fromList a
        again = Set.size s < n
    in
        if again then (setStep n nseed gen) else (Set.toList s,nseed)


randomCoor : Int -> Int -> Random.Generator (Int, Int)
randomCoor h w =
    Random.pair (Random.int 0 <| h - 1) (Random.int 0 <| w - 1)


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
    in
        gridp


decode : Decode.Decoder Model
decode =
    Decode.map7
        (\status grid games wins start curr flags ->
            { status = status
            , grid = grid 
            , seed = Random.initialSeed <| Time.posixToMillis curr
            , games = games
            , wins = wins
            , start = start
            , curr = curr
            , flags = flags
            }   
        )
        (Decode.field "status" (Decode.map decodeState Decode.string))
        (Decode.field "grid" (Grid.decode Color.decode))
        --(Decode.field "seed" (Decode.map decodeSeed (Decode.list Decode.int)))
        (Decode.field "games" Decode.int)
        (Decode.field "wins" Decode.int)
        (Decode.field "start" (Decode.map decodeTime Decode.int))
        (Decode.field "curr" (Decode.map decodeTime Decode.int))
        (Decode.field "flags" Decode.int)


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
            , ("flags", Encode.int model.flags)
            ]
        )


decodeState : String -> State
decodeState string =
    case string of
        "lost" ->
            Lost
        "won" ->
            Won
        "playing" ->
            Playing
        "paused" ->
            Paused
        _ ->
            New



encodeState : State -> String
encodeState state =
    case state of
        Playing ->
            "playing"
        Won ->
            "won"
        
        Paused ->
            "paused"
        
        Lost ->
            "lost"

        New ->
            "new"


decodeTime : Int -> Time.Posix
decodeTime t = Time.millisToPosix t


{-decodeSeed : List Int -> Random.Seed
decodeSeed (x :: y :: xs) =
    Random.Seed x y

encodeSeed : Random.Seed -> Encode.Value
encodeSeed (Random.Seed n0 n1) =
    Encode.list Encode.int [n0,n1]-}
