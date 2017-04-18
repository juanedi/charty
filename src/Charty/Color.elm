module Charty.Color
    exposing
        ( Color
        , assignDefaults
        )

import Array exposing (Array)
import Charty.ArrayUtil as ArrayUtil


{-| The color used to draw a line. For the moment, any string used to specify
SVG colors is valid, so things such as "red" and "#FF0000" should work.
-}
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


assignDefaults : List a -> List ( Color, a )
assignDefaults dataset =
    let
        colorCount =
            Array.length defaultColorPalette

        color index =
            ArrayUtil.unsafeGet (index % colorCount) defaultColorPalette
    in
        List.indexedMap (\i series -> ( (color i), series )) dataset
