module Charty.LineChart exposing (draw, Config)

import Charty.SelectList as SL exposing (include, maybe)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events as Events


type alias DataPoint =
    ( Float, Float )


type alias SvgPoint =
    ( Float, Float )


type alias Transform =
    DataPoint -> SvgPoint


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
            calculateTransform dataset

        lines =
            List.map (drawSeries cfg transform) dataset
    in
        svgCanvas
            [ axis transform
            , g [] lines
            ]


svgCanvas : List (Svg msg) -> Svg msg
svgCanvas content =
    let
        background =
            Svg.rect [ width "1000", height "1000", fill "#FAFAFA" ] []
    in
        Svg.svg
            [ width "100%", height "100%", viewBox "0 0 1000 1000" ]
            (background :: content)


calculateTransform : Dataset -> Transform
calculateTransform dataset =
    let
        points =
            List.concat dataset

        xs =
            List.map (\( x, y ) -> x) points

        ys =
            List.map (\( x, y ) -> y) points

        ( mXm, mXM ) =
            ( List.minimum xs, List.maximum xs )

        ( mYm, mYM ) =
            ( List.minimum ys, List.maximum ys )

        scaleFactor vm vM v =
            if vm == vM then
                0.5
            else
                (v - vm) / (vM - vm)
    in
        case ( mXm, mXM, mYm, mYM ) of
            ( Just xm, Just xM, Just ym, Just yM ) ->
                \( x, y ) ->
                    ( 50 + 900 * (scaleFactor xm xM x)
                    , 950 - 900 * (scaleFactor (Basics.min 0 ym) (Basics.max 0 yM) y)
                    )

            _ ->
                identity


axis : Transform -> Svg msg
axis transform =
    let
        ( _, zY ) =
            transform ( 0, 0 )

        axisLine ( vx1, vy1 ) ( vx2, vy2 ) =
            line [ x1 (toString vx1), y1 (toString vy1), x2 (toString vx2), y2 (toString vy2), stroke "#CACACA", strokeDasharray "5 5" ] []

        xAxis =
            axisLine ( 50, zY ) ( 950, zY )

        yAxis =
            axisLine ( 50, 50 ) ( 50, 950 )
    in
        g [] [ xAxis, yAxis ]


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
                , include <| fill "blue"
                , include <| class "charty-series-point"
                , maybe <| handle Events.onMouseOver cfg.onMouseOver
                , maybe <| handle Events.onMouseOut cfg.onMouseOut
                ]
            )
            []
