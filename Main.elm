module Main exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Msg =
    ()


type alias Model =
    Int


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( 1, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div
        [ Html.Attributes.style [ ( "width", "100%" ), ( "height", "100%" ) ] ]
        [ Html.div
            [ Html.Attributes.style [ ( "width", "600px" ), ( "margin", "0 auto" ) ] ]
            [ lineChart
                [ [ ( 150, 400 ), ( 350, 100 ), ( 550, 300 ), ( 850, 200 ) ]
                , [ ( 150, 800 ), ( 350, 500 ), ( 550, 700 ), ( 850, 600 ) ]
                ]
            ]
        ]


lineChart : List (List ( Int, Int )) -> Svg msg
lineChart series =
    svg
        [ width "100%", height "100%", viewBox "0 0 1000 1000" ]
        [ background
        , axis
        , g [] (List.map drawLine series)
        ]


background : Svg msg
background =
    rect [ width "1000", height "1000", fill "#FAFAFA" ] []


axis : Svg msg
axis =
    g []
        [ line [ x1 "50", y1 "50", x2 "50", y2 "950", stroke "#CACACA", Svg.Attributes.strokeDasharray "5 5" ] []
        , line [ x1 "50", y1 "950", x2 "950", y2 "950", stroke "#CACACA", Svg.Attributes.strokeDasharray "5 5" ] []
        ]


drawLine : List ( Int, Int ) -> Svg msg
drawLine linePoints =
    let
        attr =
            linePoints
                |> List.map (\( x, y ) -> toString x ++ " " ++ toString y)
                |> String.join ", "
    in
        polyline [ points attr, stroke "blue", fill "transparent" ] []
