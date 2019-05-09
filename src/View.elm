module View exposing (view)

import Grid exposing (..)
import Messages exposing (..)
import Model exposing (..)
import Color exposing (..)
import Html exposing (Html, div, text, button)
import Html.Events.Extra.Mouse as Mouse exposing (onClick, EventOptions, onWithOptions)
import Html.Attributes exposing (style)
import Svg exposing (..)
import Svg.Attributes as SvgAttrs

renderBox : Cell Color -> Svg Msg
renderBox cell =
    let
        color = cell.val

        x = Tuple.first cell.pos

        y = Tuple.second cell.pos

        box = Svg.rect
            [ SvgAttrs.width "30"
            , SvgAttrs.height "30"
            , SvgAttrs.fill (toString color)
            , SvgAttrs.stroke (toString <| darken 20 color)
            , SvgAttrs.strokeWidth "1"
            , SvgAttrs.x (String.fromInt x)
            , SvgAttrs.y (String.fromInt y)
            ]
            []
        text = Svg.text_
                [ SvgAttrs.fill (toString black)
                , SvgAttrs.x (String.fromInt <| x + 15)
                , SvgAttrs.y (String.fromInt <| y + 15)
                , Html.Attributes.style "font-family" "Arial, Helvetica, sans-serif"
                , Html.Attributes.style "color" (toString <| darken 10 color)
                , Html.Attributes.style "font-size" "8pt"
                ]
                [ Svg.text (String.fromInt cell.neigh)] 
    in
        --box
        Svg.g
            []
            <|
            if cell.rev then
                [box , text]
            else
                [ box ]


renderGrid : Grid Color -> Html Msg
renderGrid grid =
    List.map renderBox grid
        |>  Svg.svg
            [ SvgAttrs.width "300"
            , SvgAttrs.height "300"
            ]


onRightClick : (Mouse.Event -> msg) -> Html.Attribute msg 
onRightClick = Mouse.onWithOptions "auxclick" {stopPropagation = False, preventDefault = True}

renderGameButton : State -> Html Msg
renderGameButton state =
    let
        (txt, msg) =
            case state of 
                Playing ->
                    ("Pause", Noop)
                Lost ->
                    ("New Game",NewGame)
                Won ->
                    ("New Game",NewGame)
    in
        button
            [ Mouse.onClick (\event -> msg)
            , Html.Attributes.style "background" <| toString uncovered
            , Html.Attributes.style "color" <| toString black
            , Html.Attributes.style "display" "block"
            , Html.Attributes.style "font-family" "Arial,Helvetica,sans-serif"
            , Html.Attributes.style "font-size" "10pt"
            , Html.Attributes.style "height" "50px"
            , Html.Attributes.style "width" "100px"
            , Html.Attributes.style "border" "0"
            , Html.Attributes.style "cursor" "pointer"

            ]
            [ Html.text txt ]

view : Model -> Html Msg
view model = 
    div
        [ Html.Attributes.style "width" "600px"
        , Html.Attributes.style "height" "600px"
        , Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "left" "0"
        , Html.Attributes.style "top" "0"
        , Mouse.onClick (\event -> Reveal event.clientPos)
        , onRightClick (\event -> Flag event.clientPos)
        ]
        [ renderGrid model.grid 
        , renderGameButton model.status
        ]
