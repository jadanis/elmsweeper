import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp, onResize)

import Messages exposing (..)
import Model exposing (..)
import View exposing (..)


main =
    Browser.sandbox
        { init = Model.init
        , update = Messages.update
        , view = View.view
        }




        
