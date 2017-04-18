module LineChartExample exposing (..)

import Array
import Charty.LineChart as LineChart
import Html exposing (Html, div, text)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as Decode
import Json.Encode as Encode
import Layout


type Msg
    = DatasetChange String
    | ToggleLabels


type alias Model =
    { input : String
    , inputOk : Bool
    , dataset : LineChart.Dataset
    , drawLabels : Bool
    }


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
        dataset =
            sampleDataset
    in
        ( { input = encodeDataset dataset, inputOk = True, dataset = sampleDataset, drawLabels = True }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DatasetChange text ->
            case Decode.decodeString datasetDecoder text of
                Result.Ok dataset ->
                    ( { model | input = text, inputOk = True, dataset = dataset }, Cmd.none )

                Result.Err _ ->
                    ( { model | input = text, inputOk = False }, Cmd.none )

        ToggleLabels ->
            ( { model | drawLabels = not model.drawLabels }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        defaults =
            LineChart.defaults

        chart =
            LineChart.view { defaults | drawLabels = model.drawLabels } model.dataset

        opacity =
            if model.inputOk then
                1
            else
                0.3

        configTitle title =
            Html.div [ Attributes.style [ ( "font-weight", "bold" ), ( "margin-bottom", "10px" ) ] ] [ text title ]

        configSection title styles content =
            Html.div
                [ Attributes.style <| [ ( "margin-bottom", "20px" ), ( "position", "relative" ) ] ++ styles ]
                (configTitle title :: content)
    in
        Layout.twoColumns
            [ Html.p
                []
                [ Html.text "The dataset below will be displayed on the right upon validation." ]
            , configSection "Settings" [] <|
                [ Html.label
                    []
                    [ Html.input
                        [ Attributes.type_ "checkbox"
                        , Attributes.checked model.drawLabels
                        , Attributes.style [ ( "margin-right", "15px" ) ]
                        , Events.onCheck (always ToggleLabels)
                        ]
                        []
                    , text "display labels"
                    ]
                ]
            , configSection "Data"
                [ ( "flex-grow", "1" ) ]
                [ Html.textarea
                    [ Attributes.style
                        [ ( "position", "absolute" )
                        , ( "min-width", "100%" )
                        , ( "max-width", "100%" )
                        , ( "height", "90%" )
                        , ( "font-size", "14px" )
                        ]
                    , Events.onInput DatasetChange
                    ]
                    [ Html.text model.input ]
                ]
            ]
            (Html.div
                [ Attributes.style [ ( "opacity", toString opacity ) ] ]
                [ chart ]
            )


sampleDataset : LineChart.Dataset
sampleDataset =
    [ { label = "Series 1"
      , data = [ ( 100000, 3 ), ( 100001, 4 ), ( 100002, 3 ), ( 100003, 2 ), ( 100004, 1 ), ( 100005, 1 ), ( 100006, -1 ) ]
      }
    , { label = "Series 2"
      , data = [ ( 100000, 1 ), ( 100001, 2.5 ), ( 100002, 3 ), ( 100003, 3.5 ), ( 100004, 3 ), ( 100005, 2 ), ( 100006, 0 ) ]
      }
    , { label = "Series 3"
      , data = [ ( 100000, 2 ), ( 100001, 1.5 ), ( 100002, 0 ), ( 100003, 3 ), ( 100004, -0.5 ), ( 100005, -1.5 ), ( 100006, -2 ) ]
      }
    ]


encodeDataset : LineChart.Dataset -> String
encodeDataset dataset =
    let
        entryEncoder =
            \( x, y ) -> Encode.array (Array.fromList [ Encode.float x, Encode.float y ])

        seriesEncoder series =
            Encode.object
                [ ( "label", Encode.string series.label )
                , ( "data", series.data |> List.map entryEncoder |> Encode.list )
                ]

        datasetEncoder =
            List.map seriesEncoder >> Encode.list
    in
        Encode.encode 4 (datasetEncoder dataset)


datasetDecoder : Decode.Decoder LineChart.Dataset
datasetDecoder =
    let
        arrayToTuple a =
            case Array.toList a of
                x :: y :: [] ->
                    Decode.succeed ( x, y )

                _ ->
                    Decode.fail "Failed to decode point"

        entryDecoder =
            Decode.array Decode.float |> Decode.andThen arrayToTuple

        seriesDecoder =
            Decode.map2 (\label data -> { label = label, data = data })
                (Decode.field "label" Decode.string)
                (Decode.field "data" (Decode.list entryDecoder))
    in
        Decode.list <| seriesDecoder
