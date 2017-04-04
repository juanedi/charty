module Charty.LineChart
    exposing
        ( draw
        , Config
        , defaults
        )

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


type alias Color =
    String


type alias Config msg =
    { drawPoints : Bool
    , onMouseOver : Maybe (DataPoint -> msg)
    , onMouseOut : Maybe (DataPoint -> msg)
    , background : Color
    , colorPalette : List Color
    }


defaults : Config msg
defaults =
    { drawPoints = True
    , onMouseOver = Nothing
    , onMouseOut = Nothing
    , background = "#FAFAFA"
    , colorPalette =
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
    }


draw : Config msg -> Dataset -> Svg msg
draw cfg dataset =
    let
        transform =
            calculateTransform dataset

        seriesWithColors =
            withColors cfg.colorPalette dataset

        lines =
            List.map (uncurry <| drawLine transform) seriesWithColors

        points =
            List.map (uncurry <| drawPoints cfg transform) seriesWithColors
    in
        svgCanvas cfg.background
            [ axis transform
            , g [] lines
            , g [] points
            ]


withColors : List Color -> Dataset -> List ( Color, Series )
withColors palette dataset =
    let
        requiredColors =
            List.length dataset

        rec colorList =
            if (List.length colorList < requiredColors) then
                rec (colorList ++ palette)
            else
                List.map2 (,) colorList dataset
    in
        rec palette


svgCanvas : Color -> List (Svg msg) -> Svg msg
svgCanvas backgroundColor content =
    let
        background =
            Svg.rect [ width "1000", height "1000", fill backgroundColor ] []
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


drawLine : Transform -> Color -> Series -> Svg msg
drawLine transform color series =
    let
        pointString ( x, y ) =
            toString x ++ " " ++ toString y

        attr =
            series
                |> List.map (transform >> pointString)
                |> String.join ", "
    in
        polyline [ points attr, stroke color, fill "transparent" ] []


drawPoints : Config msg -> Transform -> Color -> Series -> Svg msg
drawPoints cfg transform color series =
    g [] <|
        if cfg.drawPoints then
            List.map (drawPoint cfg transform color) series
        else
            []


drawPoint : Config msg -> Transform -> Color -> DataPoint -> Svg msg
drawPoint cfg transform color point =
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
                , include <| fill color
                , maybe <| handle Events.onMouseOver cfg.onMouseOver
                , maybe <| handle Events.onMouseOut cfg.onMouseOut
                ]
            )
            []
