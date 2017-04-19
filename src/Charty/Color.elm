module Charty.Color
    exposing
        ( Color
        , defaultPalette
        , assignDefaults
        )

{-| Utilities for assigning colors to series/data groups.
You probably won't need to use it unless yo want to modify or use the default color paletter for other purposes.

@docs Color
@docs defaultPalette
@docs assignDefaults
-}

import Array exposing (Array)
import Charty.ArrayUtil as ArrayUtil


{-| The color used to draw a line. For the moment, any string used to specify
SVG colors is valid, so things such as "red" and "#FF0000" should work.
-}
type alias Color =
    String


{-| Default color palette.
All credits for this beautiful palette go to http://www.mulinblog.com/a-color-palette-optimized-for-data-visualization/
-}
defaultPalette : Array Color
defaultPalette =
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


{-| Assigns a color of the default palette to each element of the least.
If the list has too many items, color will be repeated.
-}
assignDefaults : List a -> List ( Color, a )
assignDefaults dataset =
    let
        colorCount =
            Array.length defaultPalette

        color index =
            ArrayUtil.unsafeGet (index % colorCount) defaultPalette
    in
        List.indexedMap (\i series -> ( (color i), series )) dataset
