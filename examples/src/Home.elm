module Home exposing (..)

import Charty.LineChart as LineChart
import Charty.PieChart as PieChart
import Html as H exposing (Html)
import Html.Attributes as HA
import Svg exposing (Svg)


type alias Msg =
    ()


type alias Model =
    ()


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
    ( (), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
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
                    [ demoLink "Line charts" sampleLineChart "Visualize single or multiple series with their corresponding labels."
                    , demoLink "Pie charts" samplePieChart "Use these to compare proportions of different slices of your data."
                    , demoLink "More comming" (icon "add") "Bars, stacked series, deeper customization comming soon."
                    ]
                ]
            ]
        ]


demoLink : String -> Html msg -> String -> Html msg
demoLink title visual description =
    H.div
        [ HA.class "col s12 m4" ]
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
