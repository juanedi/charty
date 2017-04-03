module LineChartExample exposing (..)

import Charty.LineChart
import Html exposing (Html, div, text)
import Html.Attributes


type Msg
    = SetMsg String
    | ClearMsg


type alias Model =
    Maybe String


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
    ( Nothing, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetMsg s ->
            ( Just s, Cmd.none )

        ClearMsg ->
            ( Nothing, Cmd.none )


chartConfig : Charty.LineChart.Config Msg
chartConfig =
    Charty.LineChart.defaults


view : Model -> Html Msg
view model =
    let
        data =
            [ [ ( 100000, 3 ), ( 100001, 4 ), ( 100002, 3 ), ( 100003, 2 ), ( 100004, 1 ), ( 100005, 1 ), ( 100006, -1 ) ]
            , [ ( 100000, 1 ), ( 100001, 2 ), ( 100002, 3 ), ( 100003, 4 ), ( 100004, 3 ), ( 100005, 2 ), ( 100006, 0 ) ]
            , [ ( 100000, 2 ), ( 100001, 1 ), ( 100002, 0 ), ( 100003, 3 ), ( 100004, -1 ), ( 100005, -2 ), ( 100006, -2 ) ]
            ]
    in
        Html.div
            [ Html.Attributes.style [ ( "width", "100%" ), ( "height", "100%" ) ] ]
            [ Html.div
                [ Html.Attributes.style [ ( "height", "70vh" ), ( "margin", "0 auto" ) ] ]
                [ Charty.LineChart.draw chartConfig data
                ]
            ]
