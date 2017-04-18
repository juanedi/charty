module Charty.Labels exposing (LabelEntry, withLabels)

import Charty.Color exposing (Color)
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias LabelEntry =
    ( Color, String )


type alias Config =
    { background : Color
    , labelsColor : Color
    }


withLabels : Config -> List LabelEntry -> Svg msg -> Svg msg
withLabels config labels chart =
    let
        background =
            Svg.rect [ width "1450", height "1000", fill config.background ] []
    in
        Svg.svg
            [ viewBox "0 0 1450 1000" ]
            [ background, chart, drawLabels config labels ]


drawLabels : Config -> List LabelEntry -> Svg msg
drawLabels config slices =
    let
        labels slices =
            List.indexedMap (labelRow config) slices
    in
        Svg.g [] (labels slices)


labelRow : Config -> Int -> LabelEntry -> Svg msg
labelRow config index ( color, label ) =
    let
        xBase =
            1000 + 50

        paddingTop =
            100 + (index * 70)

        colorDimensions =
            30

        displayText =
            if String.length label > 30 then
                (String.left 27 label) ++ "..."
            else
                label
    in
        Svg.g
            []
            [ Svg.rect
                [ x <| toString xBase
                , y <| toString (paddingTop - (floor <| colorDimensions / 2))
                , width <| toString colorDimensions
                , height <| toString colorDimensions
                , fill color
                ]
                []
            , text_
                [ x <| toString (xBase + colorDimensions + 20)
                , y <| toString paddingTop
                , fill config.labelsColor
                , fontFamily "sans-serif"
                , fontSize "25px"
                , alignmentBaseline "middle"
                ]
                [ text displayText ]
            ]
