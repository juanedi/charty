module Layout exposing (twoColumns)

import Html as H exposing (Html)
import Html.Attributes as HA
import VirtualDom exposing (Node)


twoColumns : List (Html msg) -> Node msg -> Html msg
twoColumns controls chart =
    H.div
        [ HA.class "demo-two-columns" ]
        [ H.div [ HA.class "controls" ] controls
        , H.div [ HA.class "chart" ] [ chart ]
        ]
