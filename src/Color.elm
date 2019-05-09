module Color exposing (..)


type Color
    = Color { red : Int, green : Int, blue : Int }


rgb : Int -> Int -> Int -> Color
rgb red green blue =
    Color { red = red, green = green , blue = blue }


toRgb : Color -> {red : Int, green : Int, blue : Int }
toRgb (Color rawRgb) =
    rawRgb


toString : Color -> String
toString (Color {red, green, blue}) =
    "rgb("
        ++ String.fromInt red
        ++ ","
        ++ String.fromInt green
        ++ ","
        ++ String.fromInt blue
        ++ ")"


covered = rgb 193 193 193
flagged = rgb 255 200 75
uncovered = rgb 233 233 233
exploded = rgb 255 65 30
black = rgb 0 0 0
white = rgb 255 255 255

darken : Int -> Color -> Color
darken st color =
    let
        r_color = toRgb color
        red = r_color.red - st
        green = r_color.green - st
        blue = r_color.blue - st
    in
        rgb red green blue

