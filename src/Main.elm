
import Browser
import Messages exposing (..)
import Model exposing (..)
import View exposing (..)
import Time
import Task
import Random

main =
    Browser.element
        { init = init
        , update = Messages.update
        , view = View.view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick

init t = 
    let
        start = Time.millisToPosix t
        seed = Random.initialSeed (modBy 100 t)
        (grid,nseed) =
            Model.newGame seed
        i = Model.init
        model = { i | start = start, curr = start, seed = nseed , grid = grid}
    in
        (model, Cmd.none)
