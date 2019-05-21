
import Browser
import Messages exposing (Msg(..),update,save)
import Model exposing (Model,init,decode)
import View exposing (view)
import Time
import Task
import Random
import Json.Decode as Decode
import Json.Encode as Encode


main =
    Browser.element
        { init = 
            ( \value ->
                let
                    res = Decode.decodeValue Model.decode value
                    model = Result.withDefault Model.init res
                    cmd = 
                        case res of
                            Ok m ->
                                Cmd.none
                            Err e ->
                                save <| Decode.errorToString e
                in
                    (model, cmd)
            )
        , update = Messages.update
        , view = View.view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick
