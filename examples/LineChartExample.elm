module LineChartExample exposing (..)

import Array
import Charty.LineChart as LC
import Html as H exposing (Html, div, text)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import Json.Encode as JE


type Msg
    = DatasetChange String


type alias Model =
    LC.Dataset


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
    ( sampleDataset, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update (DatasetChange text) model =
    case JD.decodeString datasetDecoder text of
        Result.Ok dataset ->
            ( dataset, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    H.div
        [ HA.style
            [ ( "display", "flex" )
            , ( "height", "100vh" )
            , ( "background-color", "#FAFAFA" )
            ]
        ]
        [ H.div
            [ HA.style [ ( "padding", "10px" ), ( "display", "flex" ), ( "flex-direction", "column" ) ] ]
            [ H.textarea
                [ HA.style [ ( "flex-grow", "1" ), ( "min-width", "500px" ), ( "font-size", "15px" ) ]
                , HE.onInput DatasetChange
                ]
                [ H.text (JE.encode 4 (datasetEncoder model)) ]
            ]
        , H.div
            [ HA.style [ ( "flex-grow", "1" ) ] ]
            [ LC.draw LC.defaults model ]
        ]


sampleDataset : LC.Dataset
sampleDataset =
    [ [ ( 100000, 3 ), ( 100001, 4 ), ( 100002, 3 ), ( 100003, 2 ), ( 100004, 1 ), ( 100005, 1 ), ( 100006, -1 ) ]
    , [ ( 100000, 1 ), ( 100001, 2.5 ), ( 100002, 3 ), ( 100003, 3.5 ), ( 100004, 3 ), ( 100005, 2 ), ( 100006, 0 ) ]
    , [ ( 100000, 2 ), ( 100001, 1.5 ), ( 100002, 0 ), ( 100003, 3 ), ( 100004, -0.5 ), ( 100005, -1.5 ), ( 100006, -2 ) ]
    ]


datasetEncoder : LC.Dataset -> JE.Value
datasetEncoder =
    let
        entryEncoder =
            \( x, y ) -> JE.array (Array.fromList [ JE.float x, JE.float y ])

        seriesEncoder =
            List.map entryEncoder >> JE.list
    in
        List.map seriesEncoder >> JE.list


datasetDecoder : JD.Decoder LC.Dataset
datasetDecoder =
    let
        arrayToTuple a =
            case Array.toList a of
                x :: y :: [] ->
                    JD.succeed ( x, y )

                _ ->
                    JD.fail "Failed to decode point"

        entryDecoder =
            JD.array JD.float |> JD.andThen arrayToTuple
    in
        JD.list <| JD.list entryDecoder
