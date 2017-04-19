module Main exposing (..)

import Charty.LineChart as LineChart
import Charty.PieChart as PieChart
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import LineChartExample
import PieChartExample
import Svg exposing (Svg)


type Msg
    = NavigateToLanding
    | NavigateToLineChart
    | NavigateToPieChart
    | LineChartExampleMsg LineChartExample.Msg
    | PieChartExampleMsg PieChartExample.Msg


type Model
    = Landing
    | LineChartExample LineChartExample.Model
    | PieChartExample PieChartExample.Model


main : Program Never Model Msg
main =
    H.program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( Landing, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( _, NavigateToLanding ) ->
            ( Landing, Cmd.none )

        ( _, NavigateToLineChart ) ->
            ( LineChartExample (LineChartExample.init), Cmd.none )

        ( _, NavigateToPieChart ) ->
            ( PieChartExample (PieChartExample.init), Cmd.none )

        ( LineChartExample model, LineChartExampleMsg msg ) ->
            LineChartExample.update msg model
                |> Tuple.mapFirst LineChartExample
                |> Tuple.mapSecond (Cmd.map LineChartExampleMsg)

        ( PieChartExample model, PieChartExampleMsg msg ) ->
            PieChartExample.update msg model
                |> Tuple.mapFirst PieChartExample
                |> Tuple.mapSecond (Cmd.map PieChartExampleMsg)

        _ ->
            Debug.crash "Unexpected msg"


view : Model -> Html Msg
view model =
    case model of
        Landing ->
            landing

        LineChartExample model ->
            example NavigateToLineChart <| H.map LineChartExampleMsg (LineChartExample.view model)

        PieChartExample model ->
            example NavigateToPieChart <| H.map PieChartExampleMsg (PieChartExample.view model)


example : Msg -> Html Msg -> Html Msg
example currentMenuLink content =
    let
        menuItem link label =
            H.li
                [ HA.classList [ ( "active", link == currentMenuLink ) ]
                ]
                [ H.a
                    [ HA.href "#", HE.onClick link ]
                    [ H.text label ]
                ]
    in
        H.div
            []
            [ H.nav
                [ HA.class "indigo darken-1" ]
                [ H.div
                    [ HA.class "nav-wrapper container" ]
                    [ H.a [ HA.class "brand-logo", HA.href "#", HE.onClick NavigateToLanding ] [ H.text "Charty" ]
                    , H.ul
                        [ HA.class "right hide-on-med-and-down" ]
                        [ menuItem NavigateToLineChart "Line Chart"
                        , menuItem NavigateToPieChart "Pie Chart"
                        ]
                    ]
                ]
            , H.div
                [ HA.class "section container chart-demo" ]
                [ content ]
            ]


landing : Html Msg
landing =
    H.div
        []
        [ H.nav
            [ HA.class "indigo darken-1" ]
            [ H.div
                [ HA.class "nav-wrapper container" ]
                []
            ]
        , H.div
            [ HA.class "section" ]
            [ H.div
                [ HA.class "container" ]
                [ H.h1 [ HA.class "header center" ] [ H.text "Charty" ]
                , H.div [ HA.class "row center" ]
                    [ H.h5 [ HA.class "header col s12 light" ] [ H.text "A simple SVG charts library for Elm." ]
                    ]
                , H.div [ HA.class "row center links" ]
                    [ H.a [ HA.href "http://package.elm-lang.org/packages/juanedi/charty/latest", HA.class "btn-large indigo" ] [ H.text "Documentation" ]
                    , H.a [ HA.href "https://github.com/juanedi/charty", HA.class "btn-large indigo" ] [ H.text "View source" ]
                    ]
                ]
            ]
        , H.div
            [ HA.class "container" ]
            [ H.div
                [ HA.class "section" ]
                [ H.div
                    [ HA.class "row" ]
                    [ demoLink (Just NavigateToLineChart) "Line charts" sampleLineChart "Visualize single or multiple series with their corresponding labels."
                    , demoLink (Just NavigateToPieChart) "Pie charts" samplePieChart "Use these to compare proportions of different slices of your data."
                    , demoLink Nothing "More comming" (icon "add") "Bars, stacked series, deeper customization comming soon."
                    ]
                ]
            ]
        ]


demoLink : Maybe Msg -> String -> Html Msg -> String -> Html Msg
demoLink onClick title visual description =
    let
        linkAttributes =
            onClick
                |> Maybe.map (\msg -> [ HE.onClick msg, HA.style [ ( "cursor", "pointer" ) ] ])
                |> Maybe.withDefault []
    in
        H.div
            (HA.class "col s12 m4" :: linkAttributes)
            [ H.div
                [ HA.class "icon-block" ]
                [ H.h2 [ HA.class "center indigo-text" ]
                    [ H.div [ HA.class "sample-chart valign-wrapper" ] [ visual ]
                    ]
                , H.h5 [ HA.class "center" ] [ H.text title ]
                , H.p [ HA.class "light" ] [ H.text description ]
                ]
            ]


icon : String -> Html msg
icon name =
    H.i [ HA.class "material-icons" ] [ H.text name ]


sampleLineChart : Svg msg
sampleLineChart =
    let
        dataset =
            [ { label = "Series 1", data = [ ( 0, 1 ), ( 1, 2 ), ( 2, 3 ) ] }
            , { label = "Series 2", data = [ ( 0, 3 ), ( 1, 1 ), ( 2, 4 ) ] }
            , { label = "Series 3", data = [ ( 0, 2 ), ( 1, 3 ), ( 2, 1 ) ] }
            ]

        defaults =
            LineChart.defaults
    in
        LineChart.view
            { defaults
                | drawLabels = False
                , labelPrecision = 0
                , background = "#FFFFFF"
            }
            dataset


samplePieChart : Svg msg
samplePieChart =
    let
        dataset =
            [ ( "Group A", 20 )
            , ( "Group B", 10 )
            , ( "Group C", 70 )
            ]

        defaults =
            PieChart.defaults
    in
        PieChart.view
            { defaults | background = "#FFFFFF" }
            dataset
