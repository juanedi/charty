module LineChartExample exposing (..)

import Charty
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


chartCfg =
    Charty.config
        { drawPoints = True
        , onMouseOver = Nothing
        , onMouseOut = Nothing
        }


view : Model -> Html Msg
view model =
    Html.div
        [ Html.Attributes.style [ ( "width", "100%" ), ( "height", "100%" ) ] ]
        [ Html.div
            [ Html.Attributes.style [ ( "height", "70vh" ), ( "margin", "0 auto" ) ] ]
            [ Charty.lineChart
                chartCfg
                [ [ ( 150, 400 ), ( 350, 100 ), ( 550, 300 ), ( 850, 200 ) ]
                , [ ( 150, 800 ), ( 350, 500 ), ( 550, 700 ), ( 850, 600 ) ]
                ]
            ]
        , Html.div [] [ Html.text (Maybe.withDefault "Boo!" model) ]
        ]
