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
import Time

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
            , SvgAttrs.stroke (toString white)
            , SvgAttrs.strokeWidth "2"
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
            , Html.Attributes.style "background" <| toString covered
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


renderLabel : String -> Html Msg
renderLabel str =
    div
        [ Html.Attributes.style "color" <| toString <| covered
        , Html.Attributes.style "font-weight" "300"
        , Html.Attributes.style "line-height" "1"
        , Html.Attributes.style "margin" "30px 0 0"
        ]
        [ Html.text str ]


renderTitle : String -> Html Msg
renderTitle str =
    div
        [ Html.Attributes.style "color" <| toString <| darken 20 flagged
        , Html.Attributes.style "font-size" "40px"
        , Html.Attributes.style "line-height" "60px"
        , Html.Attributes.style "margin" "30px 0 0"
        ]
        [ Html.text str ]


renderTime : Time.Posix -> Time.Posix -> Html Msg
renderTime start curr =
    let
        ellapse = Time.millisToPosix <| (Time.posixToMillis curr) - (Time.posixToMillis start)
        minute = Time.toMinute Time.utc ellapse
        second = Time.toSecond Time.utc ellapse
        txt = (String.fromInt minute) ++ ":" ++ (if second < 10 then "0" else "") ++ (String.fromInt second)
    in
        renderLabel txt

renderPanel : Model -> Html Msg
renderPanel model =
    div
    [ Html.Attributes.style "bottom" "80px"
    , Html.Attributes.style "color" <| toString covered
    , Html.Attributes.style "font-family" "Arial, Helvetica, sans-serif"
    , Html.Attributes.style "font-size" "14pt"
    , Html.Attributes.style "left" "300px"
    , Html.Attributes.style "padding" "0 30px"
    , Html.Attributes.style "position" "absolute"
    , Html.Attributes.style "right" "0"
    , Html.Attributes.style "top" "0"
    ]
    [ renderTitle "Elm Sweeper"
    , renderLabel "Games"
    , renderLabel <| String.fromInt model.games
    , renderLabel "Wins"
    , renderLabel <| String.fromInt model.wins
    , renderLabel "Time"
    , renderTime model.start model.curr
    , renderGameButton model.status  
    ]


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
        , renderPanel model
        ]
