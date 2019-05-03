module View exposing (view)

import Color exposing (Color)
import Grid exposing (Grid)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (on, onClick, onMouseDown, onMouseUp)
import Json.Decode as Json
import Markdown
import Messages exposing(Msg(..))
import Model exposing (Model)
import Svg
import Svg.Attributes as SvgAttrs 


