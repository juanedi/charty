module Charty.PieChart
    exposing
        ( Dataset
        , view
        )

import Array
import Charty.ArrayUtil as ArrayUtil
import Charty.Common as Common
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Color =
    String


type alias Slice =
    { label : String
    , percentage : Float
    , color : Color
    }


type alias Dataset =
    List ( String, Float )


normalize : Dataset -> Dataset
normalize dataset =
    let
        valueSum =
            dataset
                |> List.map (\( label, value ) -> value)
                |> List.sum
    in
        List.map (\( label, value ) -> ( label, 100 * value / valueSum )) dataset


assignColors : Dataset -> List Slice
assignColors dataset =
    let
        colorCount =
            Array.length Common.defaultColorPalette

        assignColor i ( label, value ) =
            { label = label
            , color = ArrayUtil.unsafeGet (i % colorCount) Common.defaultColorPalette
            , percentage = value
            }
    in
        dataset
            |> List.indexedMap assignColor


accumulateStart : Float -> List Slice -> List ( Float, Slice )
accumulateStart start slices =
    case slices of
        [] ->
            []

        s :: ss ->
            ( start, s ) :: (accumulateStart (start + s.percentage) ss)


drawSlice : Float -> Slice -> Svg msg
drawSlice start slice =
    let
        ( x1, y1 ) =
            circumferencePoint start

        ( x2, y2 ) =
            circumferencePoint (start + slice.percentage)

        largeArc =
            if slice.percentage <= 50 then
                "0"
            else
                "1"

        pathDefinition =
            (List.concat >> String.join " ")
                [ -- start at center
                  [ "M 500 500" ]

                -- straight line to point 1
                , [ "L ", toString x1, toString y1 ]

                -- arc definition
                , [ "A 500 500 0", largeArc, "1" ]

                -- arc to point 2.
                -- self closing arcs (100%) are not drawn by the browser.
                -- the (hacky) solution is to use floor to prevent the arc to close itself and rely on the stroke to hide the gap.
                , [ x2 |> floor |> toString, y2 |> floor |> toString ]

                -- return to center
                , [ "Z" ]
                ]
    in
        Svg.path
            [ d pathDefinition
            , stroke slice.color
            , fill slice.color
            ]
            []


{-| Returns the coordinates of the point in the circumference that corresponds to the percentage.
-}
circumferencePoint : Float -> ( Float, Float )
circumferencePoint percentage =
    let
        ang =
            percentage * (2 * pi) / 100
    in
        ( 500 * (1 + sin ang), 500 * (1 - cos ang) )


view : Dataset -> Svg msg
view dataset =
    let
        background =
            Svg.rect [ width "1450", height "1000", fill "#FAFAFA" ] []
    in
        Svg.svg
            [ viewBox "0 0 1450 1000" ]
            [ background, slices dataset, labels dataset ]


labels : Dataset -> Svg msg
labels dataset =
    let
        slices =
            dataset
                |> List.sortBy (\( label, _ ) -> label)
                |> assignColors

        labels slices =
            List.indexedMap labelRow slices
    in
        Svg.g [] <|
            labels slices


labelRow : Int -> Slice -> Svg msg
labelRow index slice =
    let
        xBase =
            1000 + 50

        paddingTop =
            100 + (index * 70)

        colorDimensions =
            30

        displayText =
            if String.length slice.label > 30 then
                (String.left 27 slice.label) ++ "..."
            else
                slice.label
    in
        Svg.g
            []
            [ Svg.rect
                [ x <| toString xBase
                , y <| toString (paddingTop - (floor <| colorDimensions / 2))
                , width <| toString colorDimensions
                , height <| toString colorDimensions
                , fill slice.color
                ]
                []
            , text_
                [ x <| toString (xBase + colorDimensions + 20)
                , y <| toString paddingTop
                , fontFamily "sans-serif"
                , fontSize "25px"
                , alignmentBaseline "middle"
                ]
                [ text displayText ]
            ]


slices : Dataset -> Svg msg
slices dataset =
    dataset
        |> normalize
        |> List.sortBy (\( _, value ) -> value)
        |> assignColors
        |> accumulateStart 0
        |> List.map (uncurry drawSlice)
        |> Svg.svg [ viewBox "0 0 1000 1000", width "1000" ]
