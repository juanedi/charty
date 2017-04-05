module Charty.LineChart
    exposing
        ( draw
        , Config
        , Dataset
        , defaults
        )

import Array exposing (Array)
import Charty.ArrayUtil as ArrayUtil
import Charty.SelectList as SL exposing (include, maybe)
import Regex
import Round
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


type alias Config =
    { drawPoints : Bool
    , background : Color
    , colorPalette : List Color
    }


type alias DatasetStats =
    { padding : Padding
    , transform : Transform
    , yLabels : Array Float
    }


type alias Padding =
    { top : Float
    , right : Float
    , bottom : Float
    , left : Float
    }


type alias DatasetBounds =
    { xMin : Float
    , xMax : Float
    , yMin : Float
    , yMax : Float
    }


defaults : Config
defaults =
    { drawPoints = True
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


draw : Config -> Dataset -> Svg msg
draw cfg dataset =
    let
        highlight =
            ( 100002, 3 )

        stats =
            initStats dataset

        seriesWithColors =
            withColors cfg.colorPalette dataset

        lines =
            List.map (uncurry <| drawLine stats.transform) seriesWithColors

        points =
            if cfg.drawPoints then
                List.map (uncurry <| drawPoints stats.transform) seriesWithColors
            else
                []
    in
        svgCanvas cfg.background
            [ axis stats
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


initStats : Dataset -> DatasetStats
initStats dataset =
    let
        points =
            List.concat dataset

        xs =
            List.map (\( x, y ) -> x) points

        ys =
            List.map (\( x, y ) -> y) points
    in
        case ( List.minimum xs, List.maximum xs, List.minimum ys, List.maximum ys ) of
            ( Just xMin, Just xMax, Just yMin, Just yMax ) ->
                let
                    yLabels =
                        initYLabels yMin yMax

                    bounds =
                        { xMin = xMin
                        , xMax = xMax
                        , yMin = ArrayUtil.unsafeFirst yLabels
                        , yMax = ArrayUtil.unsafeLast yLabels
                        }

                    padding =
                        initPadding yLabels
                in
                    { padding = padding
                    , transform = initTransform bounds padding
                    , yLabels = yLabels
                    }

            _ ->
                let
                    padding =
                        { top = 50, right = 50, bottom = 50, left = 50 }
                in
                    { padding = padding
                    , transform = initTransform { xMin = 0, xMax = 0, yMin = 0, yMax = 0 } padding
                    , yLabels = Array.fromList [ 0 ]
                    }


initYLabels : Float -> Float -> Array Float
initYLabels yMin yMax =
    if yMin == yMax then
        [ 0, yMin, 2 * yMin ]
            |> List.sort
            |> Array.fromList
    else
        let
            step =
                (yMax - yMin) / 6

            lowerBound =
                step * (toFloat <| floor (yMin / step))

            upperBound =
                step * (toFloat <| ceiling (yMax / step))
        in
            range step lowerBound upperBound


initPadding : Array Float -> Padding
initPadding yLabels =
    let
        labelOffset n =
            -- TODO: find a proper way to decide the svg offset for a given string
            label n
                |> String.length
                |> \n -> (toFloat n * 8 * 1000) / 320

        leftOffset =
            yLabels
                |> Array.map labelOffset
                |> ArrayUtil.unsafeMaximum
    in
        { top = 50
        , right = 50
        , bottom = 50
        , left = leftOffset
        }


label : Float -> String
label =
    Round.ceiling 2 >> Regex.replace Regex.All (Regex.regex "\\.0+$") (always "")


initTransform : DatasetBounds -> Padding -> Transform
initTransform { xMin, xMax, yMin, yMax } { top, right, bottom, left } =
    let
        drawingWidth =
            1000 - right - left

        drawingHeight =
            1000 - top - bottom

        scaleFactor vm vM v =
            if vm == vM then
                0.5
            else
                (v - vm) / (vM - vm)
    in
        \( x, y ) ->
            ( left + drawingWidth * (scaleFactor xMin xMax x)
            , (1000 - top) - drawingHeight * (scaleFactor yMin yMax y)
            )


axis : DatasetStats -> Svg msg
axis stats =
    let
        { top, right, bottom, left } =
            stats.padding

        axisLine ( vx1, vy1 ) ( vx2, vy2 ) =
            line
                [ x1 <| toString vx1
                , y1 <| toString vy1
                , x2 <| toString vx2
                , y2 <| toString vy2
                , stroke "#CFCFCF"
                , strokeDasharray "5 5"
                ]
                []

        referenceLine yVal =
            let
                yT =
                    stats.transform ( 0, yVal ) |> Tuple.second
            in
                g []
                    [ axisLine ( left, yT ) ( 1000 - right, yT )
                    , text_
                        [ x "20"
                        , y <| toString (yT + 8)
                        , fontFamily "Oxygen,Helvetica,Arial,sans-serif"
                        , fontSize "24px"
                        , fill "#CFCFCF"
                        ]
                        [ text (label yVal) ]
                    ]

        yLabels =
            Array.foldr (\l r -> (referenceLine l) :: r) [] stats.yLabels

        yAxis =
            axisLine ( left, bottom ) ( left, 1000 - top )
    in
        g [] (yAxis :: yLabels)


range : Float -> Float -> Float -> Array Float
range step lowerBound upperBound =
    let
        rangeRec start end step result =
            if start <= end then
                rangeRec (start + step) end step (start :: result)
            else
                result

        range start end step =
            List.reverse <| rangeRec start end step []
    in
        Array.fromList <| range lowerBound upperBound step


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


drawPoints : Transform -> Color -> Series -> Svg msg
drawPoints transform color series =
    g [] <|
        List.map (drawPoint transform color) series


drawPoint : Transform -> Color -> DataPoint -> Svg msg
drawPoint transform color point =
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
                ]
            )
            []
