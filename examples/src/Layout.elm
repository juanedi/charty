module Layout exposing (twoColumns)

import Html exposing (Html, div)
import Html.Attributes as Attributes
import VirtualDom exposing (Node)


twoColumns : List (Html msg) -> Node msg -> Html msg
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
                [ ( "flex-grow", "1" )
                , ( "padding-left", "100px" )
                ]
            ]
            [ chart ]
        ]
