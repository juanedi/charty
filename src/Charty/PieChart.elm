module Charty.PieChart
    exposing
        ( Dataset
        , Group
        , Config
        , defaults
        , view
        )

{-| This module is in charge of drawing pie charts.

# Data representation
@docs Dataset
@docs Group

# Settings
@docs Config
@docs defaults

# Drawing
@docs view
-}

import Charty.Color as Color exposing (Color)
import Charty.Labels as Labels exposing (LabelEntry)
import Svg exposing (..)
import Svg.Attributes exposing (..)


{-| A dataset is just a list of groups.
-}
type alias Dataset =
    List Group


{-| A group that will be drawn in the chart. Note that depending on the
`maxGroupCount` setting, a group may end up being grouped with others in an
"Others" slice of the pie.
-}
type alias Group =
    { label : String, value : Float }


{-| Configuration for how the chart will be drawn. Note that
[`PieChart.defaults`](Charty-PieChart#defaults) can be used as a base
configuration.

If the color assignment is changed and a `maxGroupCount` is specified, keep in
mind that the dataset passed to the function may have some of your groups merged
into a single "Others" category.
-}
type alias Config =
    { background : Color
    , labelsColor : Color
    , maxGroupCount : Maybe Int
    , colorAssignment : Dataset -> List ( Color, Group )
    }


type alias Slice =
    { label : String
    , percentage : Float
    , color : Color
    }


{-| Default configuration. At most 8 slices will be drawn and a default color
palette is used.
-}
defaults : Config
defaults =
    { background = "transparent"
    , labelsColor = "#333333"
    , maxGroupCount = Just 8
    , colorAssignment = Color.assignDefaults
    }


{-| This function generates svg markup for the chart, provided a the necessary
configuration and dataset. Example usage:

    sampleDataset : PieChart.Dataset
    sampleDataset =
        [ ( "Group A", 40.0 )
        , ( "Group B", 25.0 )
        , ( "Group C", 35.0 )
        ]

    view : Model -> Html Msg
    view model =
        Html.div
          []
          [ Html.p [] [ Html.text "Wow!" ]
          , PieChart.view PieChart.defaults dataset
          ]
-}
view : Config -> Dataset -> Svg msg
view config dataset =
    let
        slices =
            preprocess config dataset
    in
        Labels.withLabels
            { background = config.background
            , labelsColor = config.labelsColor
            }
            (List.map label slices)
            (drawSlices config slices)


label : Slice -> LabelEntry
label s =
    ( s.color, s.label )


preprocess : Config -> Dataset -> List Slice
preprocess config dataset =
    dataset
        |> normalize
        |> List.sortBy (\{ value } -> -value)
        |> truncate config.maxGroupCount
        |> config.colorAssignment
        |> List.map
            (\( color, { label, value } ) ->
                { color = color
                , label = label
                , percentage = value
                }
            )


normalize : Dataset -> Dataset
normalize dataset =
    let
        sum =
            sumValues dataset
    in
        List.map (\{ label, value } -> { label = label, value = 100 * value / sum }) dataset


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
                        ++ [ { label = "Other", value = sumValues rest } ]


sumValues : Dataset -> Float
sumValues =
    List.foldr (\{ value } s -> value + s) 0


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


drawSlices : Config -> List Slice -> Svg msg
drawSlices config slices =
    slices
        |> accumulateStart 0
        |> List.map (uncurry (drawSlice config))
        |> Svg.svg [ viewBox "0 0 1000 1000", width "1000" ]


accumulateStart : Float -> List Slice -> List ( Float, Slice )
accumulateStart start slices =
    case slices of
        [] ->
            []

        s :: ss ->
            ( start, s ) :: (accumulateStart (start + s.percentage) ss)
