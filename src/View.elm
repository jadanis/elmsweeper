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
import Time exposing (posixToMillis)


view : Model -> Html Msg
view model = 
    div
        [ Html.Attributes.style "width" "600px"
        , Html.Attributes.style "height" "600px"
        , Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "left" "0"
        , Html.Attributes.style "top" "0"
        ]
        [ renderGrid model.grid 
        , renderPanel model
        ]


renderGrid : Grid Color -> Html Msg
renderGrid grid =
    List.map renderBox grid
        |>  Svg.svg
            [ Html.Attributes.style "margin" "5px"
            , SvgAttrs.width "300"
            , SvgAttrs.height "300"
            , Mouse.onClick (\event -> Reveal event.clientPos)
            , onRightClick (\event -> Flag event.clientPos)
            ]


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
    , renderLabel "Grade"
    , renderLabel <| grade model
    , renderLabel "Time"
    , renderTime model.start model.curr
    , renderGameButton <| actionButton model.status  
    , renderGameButton ("Reset",Reset)
    ]


onRightClick : (Mouse.Event -> msg) -> Html.Attribute msg 
onRightClick = Mouse.onWithOptions "auxclick" {stopPropagation = False, preventDefault = True}


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
                

renderGameButton : (String, Msg) -> Html Msg
renderGameButton (txt, msg) =
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
        , Html.Attributes.style "margin" "5px 0px"
        ]
        [ Html.text txt ]


actionButton : State -> (String, Msg)
actionButton state = 
    case state of 
        Playing ->
            ("Pause", Pause)
                
        Lost ->
            ("New Game",NewGame)
                
        Paused ->
            ("Continue", Continue)
                
        Won ->
            ("New Game",NewGame)
                
        New ->
            ("Pause",Pause)    


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
        [ Html.Attributes.style "color" <| toString <| flagged
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


{-
    For the current set up height and width are
    static of 10 and 10.
    The denisty is also currently static at 20 mines (thus 0.2 density).
    Find where on the bell curve the current number of wins are,
    given the expected value and variance.
    Check this against various letter grade scores.
-}
grade : Model -> String
grade model =
    let
        wins = model.wins
        games = model.games
        (ex,v) =
            find_norm (toFloat games) 10 10 0.2
        score = cum_std_norm_dist (toFloat wins) ex v
        a_plus = 0.96
        a = 0.90
        b = 0.80
        c = 0.70
        d = 0.60
        f = 0.45
    in
        {--}case (compare score f) of
            LT ->
                "Boo!"
            _ ->
                case (compare score d) of
                    LT ->
                        "F"
                    _ ->
                        case (compare score c) of
                            LT ->
                                "D"
                            _ ->
                                case (compare score b) of
                                    LT ->
                                        "C"
                                    _ ->
                                        case (compare score a) of
                                            LT ->
                                                "B"
                                            _ ->
                                                case (compare score a_plus) of
                                                    LT -> 
                                                        "A"
                                                    _ ->
                                                        "Wow! A+ !" --}
        --String.fromFloat score


-- approximation of CDF of normal dist.
-- see https://stackoverflow.com/questions/5259421/cumulative-distribution-function-in-javascript
cum_std_norm_dist : Float -> Float -> Float -> Float
cum_std_norm_dist to mean var =
    let
        z = (to - mean) / (sqrt (2 * var))
        t = 1 / (1 + 0.3275911 * z)
        a1 = 0.254829592
        a2 = -0.284496736
        a3 = 1.421413741
        a4 = -1.453152027
        a5 = 1.061405429
        erf = 1 - ((((a5*t+a4)*t + a3)*t+a2)*t+a1) * t / Basics.e^(z^2)

    in
        0.5 *(1 + erf) 


{- 
    For a given number of trials (games),
    determine the expected number of wins
    and the variance. Assuming the probability
    of a win corresponds to an intial 0. See below
-}
find_norm : Float -> Float -> Float -> Float -> (Float,Float)
find_norm t h w d =
    let
        p = find_p h w d
        expected = t * p
        variance = t * p * (1-p)
    in
        (expected,variance)

{-
    Given the height and width of a grid
    and the "density" of the mines
    find the probability of a random cell 
    having 0 neighbors
    The assumption is, that given an inital cell with 0 neighbors
    the game should be solvable. This is not the case as is.
-}
find_p : Float -> Float -> Float -> Float
find_p h w d =
    let
        size = h * w
        corner = (1-d)^4
        side = (1-d)^6
        body = (1-d)^9
    in
        (4*corner + 2*(w-2)*side + 2*(h-2)*side + (w-2)*(h-2)*body)/size

    
