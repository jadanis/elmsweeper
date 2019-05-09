import Browser
import Messages exposing (..)
import Model exposing (..)
import View exposing (..)


main =
    Browser.sandbox
        { init = Model.init
        , update = Messages.update
        , view = View.view
        }




        
