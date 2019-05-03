module Model exposing
    ( Model
    , State(..)
    , decode
    , encode
    , initial
    )


import Color exposing (Color)
import Grid exposing (Grid)
import Json.Decode as Decode 
import Json.Encode as Encode 
import Random
import MineField exposing (random)

type State
    = Playing
    | Won
    | Lost


decodeState : String -> State
decodeState string =
    case string of
        "playing" ->
            Playing
        
        "won" ->
            Won
        
        _ ->
            Lost


encodeState : State -> String
encodeState state =
    case state of
        Playing ->
            "playing"
        
        Lost ->
            "lost"
        
        Won ->
            "won"


-- Model

type alias Model = 
    { won : Bool
    , lost : Bool
    , playing : Bool
    , grid : Grid Color 
    , seed : Random.seed
     }

newGame : Random.Seed -> Model
newGame seed =
    let
        (grid, nseed) =
            MineField.random 10 10 seed
    in
        { won = False
        , lost = False
        , playing = True 
        , grid = grid
        , seed = nseed
        }

initial : Model
initial = newGame (Random.initialSeed 0)
