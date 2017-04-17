module Layout exposing (twoColumns)

import Html exposing (Html, div)
import Html.Attributes as Attributes
import Svg exposing (Svg)


twoColumns : List (Html msg) -> Svg msg -> Html msg
twoColumns controls chart =
    Html.div
        [ Attributes.style
            [ ( "display", "flex" )
            , ( "height", "100vh" )
            , ( "background-color", "#FAFAFA" )
            ]
        ]
        [ Html.div
            [ Attributes.style
                [ ( "display", "flex" )
                , ( "flex-direction", "column" )
                , ( "padding", "10px" )
                , ( "width", "370px" )
                , ( "font-family", "sans-serif" )
                ]
            ]
            controls
        , Html.div
            [ Attributes.style
                [ ( "flex-grow", "1" ) ]
            ]
            [ chart ]
        ]
