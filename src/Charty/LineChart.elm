module Charty.LineChart exposing (draw, config)

import Charty exposing (DataPoint, Transform, calculateTransform)
import Charty.SelectList as SL exposing (include, maybe)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events as Events


type Cfg msg
    = Cfg
        { drawPoints : Bool
        , onMouseOver : Maybe (DataPoint -> msg)
        , onMouseOut : Maybe (DataPoint -> msg)
        }


config record =
    Cfg record


draw : Cfg msg -> List (List DataPoint) -> Svg msg
draw cfg series =
    let
        transform =
            series |> List.concat |> calculateTransform
    in
        svg
            [ width "100%", height "100%", viewBox "0 0 1000 1000" ]
            [ background
            , axis
            , g [] (List.map (drawSeries cfg transform) series)
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


drawSeries : Cfg msg -> Transform -> List DataPoint -> Svg msg
drawSeries cfg transform linePoints =
    g [ class "charty-series" ]
        [ drawLine transform linePoints
        , drawPoints cfg transform linePoints
        ]


drawLine : Transform -> List DataPoint -> Svg msg
drawLine transform linePoints =
    let
        pointString ( x, y ) =
            toString x ++ " " ++ toString y

        attr =
            linePoints
                |> List.map (transform >> pointString)
                |> String.join ", "
    in
        polyline [ class "charty-series-line", points attr, stroke "blue", fill "transparent" ] []


drawPoints : Cfg msg -> Transform -> List DataPoint -> Svg msg
drawPoints (Cfg cfg) transform linePoints =
    g [] <|
        if cfg.drawPoints then
            List.map (drawPoint cfg transform) linePoints
        else
            []


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
