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


renderBox : Cell Color -> Html Msg
renderBox cell = 
  Svg.rect
    [ SvgAttrs.width (String.fromInt 30)
    , SvgAttrs.height (String.fromInt 30)
    , SvgAttrs.fill (Color.toString cell.val.toString)
    , SvgAttrs.stroke (Color.toString cel.val.toString)
    , SvgAttrs.strokeWidth "0.5"
    , SvgAttrs.x (String.fromInt (Tuple.first cell.pos))
    , SvgAttrs.y (String.fromInt (Tuple.second cell.pos))
    ]
    []


renderGrid : Model -> Html Msg
renderGrid {won, lost, playing, grid, seed} =
  List.map renderBox grid
