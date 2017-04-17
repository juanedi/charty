module Charty.PieChart
    exposing
        ( Dataset
        , Group
        , Color
        , Config
        , defaults
        , view
        )

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
    List Group


type alias Group =
    ( String, Float )


type alias Config =
    { background : Color
    , labelsColor : Color
    , maxGroupCount : Maybe Int
    , colorAssignment : Dataset -> List ( Color, Group )
    }


defaults : Config
defaults =
    { background = "#FAFAFA"
    , labelsColor = "#000000"
    , maxGroupCount = Just 8
    , colorAssignment = Common.withDefaultColors
    }


preprocess : Config -> Dataset -> List Slice
preprocess config dataset =
    dataset
        |> normalize
        |> List.sortBy (\( _, value ) -> -value)
        |> truncate config.maxGroupCount
        |> config.colorAssignment
        |> List.map
            (\( color, ( label, percentage ) ) ->
                { color = color
                , label = label
                , percentage = percentage
                }
            )


sumValues : Dataset -> Float
sumValues dataset =
    case dataset of
        [] ->
            0

        ( _, value ) :: rest ->
            value + (sumValues rest)


normalize : Dataset -> Dataset
normalize dataset =
    let
        sum =
            sumValues dataset
    in
        List.map (\( label, value ) -> ( label, 100 * value / sum )) dataset


truncate : Maybe Int -> Dataset -> Dataset
truncate maxGroupCount dataset =
    case maxGroupCount of
        Nothing ->
            dataset

        Just n ->
            case List.drop (n - 1) dataset of
                [] ->
                    dataset

                _ :: [] ->
                    dataset

                rest ->
                    (List.take (n - 1) dataset)
                        ++ [ ( "Other", sumValues rest ) ]


accumulateStart : Float -> List Slice -> List ( Float, Slice )
accumulateStart start slices =
    case slices of
        [] ->
            []

        s :: ss ->
            ( start, s ) :: (accumulateStart (start + s.percentage) ss)


drawSlice : Config -> Float -> Slice -> Svg msg
drawSlice config start slice =
    let
        ( x1, y1 ) =
            circumferencePoint start

        ( x2, y2 ) =
            -- self closing arcs (100%) are not drawn by the browser.
            -- this is a hack, but seems to work visually and keeps drawing code consistent.
            circumferencePoint (start + Basics.min 99.9999 slice.percentage)

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
                , [ toString x2, toString y2 ]

                -- return to center
                , [ "Z" ]
                ]
    in
        Svg.path
            [ d pathDefinition
            , stroke "transparent"
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


view : Config -> Dataset -> Svg msg
view config dataset =
    let
        background =
            Svg.rect [ width "1450", height "1000", fill config.background ] []

        slices =
            preprocess config dataset
    in
        Svg.svg
            [ viewBox "0 0 1450 1000" ]
            [ background, drawSlices config slices, drawLabels config slices ]


drawLabels : Config -> List Slice -> Svg msg
drawLabels config slices =
    let
        labels slices =
            List.indexedMap (labelRow config) slices
    in
        Svg.g [] (labels slices)


labelRow : Config -> Int -> Slice -> Svg msg
labelRow config index slice =
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
                , fill config.labelsColor
                , fontFamily "sans-serif"
                , fontSize "25px"
                , alignmentBaseline "middle"
                ]
                [ text displayText ]
            ]


drawSlices : Config -> List Slice -> Svg msg
drawSlices config slices =
    slices
        |> accumulateStart 0
        |> List.map (uncurry (drawSlice config))
        |> Svg.svg [ viewBox "0 0 1000 1000", width "1000" ]
