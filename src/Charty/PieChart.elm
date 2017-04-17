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
import Charty.Labels as Labels exposing (LabelEntry)
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Color =
    -- TODO: use common definition
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
    , labelsColor = "#333333"
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
        slices =
            preprocess config dataset

        chart =
            drawSlices config slices

        labels =
            List.map (\s -> ( s.color, s.label )) slices
    in
        Labels.withLabels { background = config.background, labelsColor = config.labelsColor } chart labels


drawSlices : Config -> List Slice -> Svg msg
drawSlices config slices =
    slices
        |> accumulateStart 0
        |> List.map (uncurry (drawSlice config))
        |> Svg.svg [ viewBox "0 0 1000 1000", width "1000" ]
