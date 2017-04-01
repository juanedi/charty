module Charty.LineChart exposing (draw, Config)

import Charty exposing (DataPoint, Transform, calculateTransform)
import Charty.SelectList as SL exposing (include, maybe)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events as Events


type alias Series =
    List DataPoint


type alias Dataset =
    List Series


type alias Config msg =
    { drawPoints : Bool
    , onMouseOver : Maybe (DataPoint -> msg)
    , onMouseOut : Maybe (DataPoint -> msg)
    }


draw : Config msg -> Dataset -> Svg msg
draw cfg dataset =
    let
        transform =
            dataset |> List.concat |> calculateTransform
    in
        svg
            [ width "100%", height "100%", viewBox "0 0 1000 1000" ]
            [ background
            , axis
            , g [] (List.map (drawSeries cfg transform) dataset)
            ]


background : Svg msg
background =
    rect [ width "1000", height "1000", fill "#FAFAFA" ] []


axis : Svg msg
axis =
    g []
        [ line [ x1 "50", y1 "50", x2 "50", y2 "950", stroke "#CACACA", strokeDasharray "5 5" ] []
        , line [ x1 "50", y1 "950", x2 "950", y2 "950", stroke "#CACACA", strokeDasharray "5 5" ] []
        ]


drawSeries : Config msg -> Transform -> Series -> Svg msg
drawSeries cfg transform series =
    g [ class "charty-series" ]
        [ drawLine transform series
        , drawPoints cfg transform series
        ]


drawLine : Transform -> Series -> Svg msg
drawLine transform series =
    let
        pointString ( x, y ) =
            toString x ++ " " ++ toString y

        attr =
            series
                |> List.map (transform >> pointString)
                |> String.join ", "
    in
        polyline [ class "charty-series-line", points attr, stroke "blue", fill "transparent" ] []


drawPoints : Config msg -> Transform -> Series -> Svg msg
drawPoints cfg transform series =
    g [] <|
        if cfg.drawPoints then
            List.map (drawPoint cfg transform) series
        else
            []


drawPoint : Config msg -> Transform -> DataPoint -> Svg msg
drawPoint cfg transform point =
    let
        ( x, y ) =
            transform point

        handle event setting =
            Maybe.map (\f -> event (f point)) setting
    in
        circle
            (SL.select
                [ include <| cx (toString x)
                , include <| cy (toString y)
                , include <| r "10"
                , include <| fill "red"
                , include <| class "charty-series-point"
                , maybe <| handle Events.onMouseOver cfg.onMouseOver
                , maybe <| handle Events.onMouseOut cfg.onMouseOut
                ]
            )
            []
