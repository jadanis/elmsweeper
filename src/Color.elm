module Color exposing (..)

import Json.Decode as Decode 
import Json.Encode as Encode 

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


covered = rgb 185 216 194
flagged = rgb 219 213 110
uncovered : Int -> Color
uncovered n = elevate n safe_green
--uncovered = rgb 103 220 213
exploded = rgb 252 119 83
black = rgb 0 0 0
white = rgb 255 255 255
safe_green = rgb 102 213 209

elevate : Int -> Color -> Color
elevate n color =
    let
        r_color = toRgb color
        red = r_color.red
        green = r_color.green
        blue = r_color.blue
        nred = red - 5
        ngreen = green - 19
        nblue = blue - 15
        ncolor = rgb nred ngreen nblue
    in
        if n == 0 then ncolor else (elevate (n-1) ncolor)

darken : Int -> Color -> Color
darken st color =
    let
        r_color = toRgb color
        red = r_color.red - st
        green = r_color.green - st
        blue = r_color.blue - st
    in
        rgb red green blue


decode : Decode.Decoder Color
decode =
    Decode.map3 rgb
        (Decode.index 0 Decode.int)
        (Decode.index 1 Decode.int)
        (Decode.index 2 Decode.int)


encode : Color -> Encode.Value
encode (Color { red, green, blue }) =
    Encode.list Encode.int [ red, green, blue ]
