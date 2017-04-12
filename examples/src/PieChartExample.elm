module PieChartExample exposing (..)

import Charty.PieChart as PieChart exposing (view)
import Html exposing (Html, div, text)


-- import Html.Attributes as Attributes
-- import Html.Events as Events


type alias Msg =
    ()


type alias Model =
    { dataset : PieChart.Dataset }


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
    let
        sampleDataset =
            [ ( "Label 1", 60 )
            , ( "Label 2", 30 )
            , ( "Label 3", 10 )
            ]
    in
        ( { dataset = sampleDataset }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    PieChart.view model.dataset
