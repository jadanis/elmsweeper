
import Browser
import Messages exposing (..)
import Model exposing (..)
import View exposing (..)
import Time
import Task
import Random

main =
    Browser.element
        { init = Model.init
        , update = Messages.update
        , view = View.view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick
