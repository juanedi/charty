module Charty.Common exposing (withDefaultColors, Color)

import Array exposing (Array)
import Charty.ArrayUtil as ArrayUtil


type alias Color =
    String


defaultColorPalette : Array Color
defaultColorPalette =
    Array.fromList
        [ "#4D4D4D" -- gray
        , "#5DA5DA" -- blue
        , "#FAA43A" -- orange
        , "#60BD68" -- green
        , "#F17CB0" -- pink
        , "#B2912F" -- brown
        , "#B276B2" -- purple
        , "#DECF3F" -- yellow
        , "#F15854" -- red
        ]


withDefaultColors : List a -> List ( Color, a )
withDefaultColors dataset =
    let
        colorCount =
            Array.length defaultColorPalette

        color index =
            ArrayUtil.unsafeGet (index % colorCount) defaultColorPalette
    in
        List.indexedMap (\i series -> ( (color i), series )) dataset
