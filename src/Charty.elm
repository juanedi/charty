module Charty exposing (lineChart, config)

import Charty.SelectList as SL exposing (include, maybe)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events as Events


type alias DataPoint =
    ( Int, Int )


type Cfg msg
    = Cfg
        { drawPoints : Bool
        , onMouseOver : Maybe (DataPoint -> msg)
        , onMouseOut : Maybe (DataPoint -> msg)
        }


config record =
    Cfg record


lineChart : Cfg msg -> List (List ( Int, Int )) -> Svg msg
lineChart cfg series =
    svg
        [ width "100%", height "100%", viewBox "0 0 1000 1000" ]
        [ background
        , axis
        , g [] (List.map (drawSeries cfg) series)
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


drawSeries : Cfg msg -> List ( Int, Int ) -> Svg msg
drawSeries cfg linePoints =
    g [ class "charty-series" ]
        [ drawLine linePoints
        , drawPoints cfg linePoints
        ]


drawLine : List ( Int, Int ) -> Svg msg
drawLine linePoints =
    let
        attr =
            linePoints
                |> List.map (\( x, y ) -> toString x ++ " " ++ toString y)
                |> String.join ", "
    in
        polyline [ class "charty-series-line", points attr, stroke "blue", fill "transparent" ] []


drawPoints : Cfg msg -> List ( Int, Int ) -> Svg msg
drawPoints (Cfg cfg) linePoints =
    g [] <|
        if cfg.drawPoints then
            List.map (drawPoint cfg) linePoints
        else
            []


drawPoint cfg point =
    let
        ( x, y ) =
            point

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
