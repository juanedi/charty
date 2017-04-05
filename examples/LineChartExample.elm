module LineChartExample exposing (..)

import Charty.LineChart as LC
import Html as H exposing (Html, div, text)
import Html.Attributes as HA
import Html.Events as HE


type Msg
    = ChangeData Model


type Model
    = Empty
    | Singleton
    | Nice


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
    ( Nice, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeData dataset ->
            ( dataset, Cmd.none )


view : Model -> Html Msg
view model =
    H.div
        [ HA.style [ ( "display", "flex" ) ] ]
        [ H.div
            [ HA.style [ ( "padding", "30px" ) ] ]
            [ datasetSelector model ]
        , H.div
            [ HA.style [ ( "flex-grow", "1" ), ( "max-height", "700px" ) ] ]
            [ LC.draw LC.defaults (dataset model) ]
        ]


datasetSelector : Model -> Html Msg
datasetSelector model =
    let
        isCurrent o =
            case ( o, model ) of
                ( Empty, Empty ) ->
                    True

                ( Singleton, Singleton ) ->
                    True

                ( Nice, Nice ) ->
                    True

                _ ->
                    False

        opt o =
            H.div
                [ HA.style [ ( "margin-bottom", "10px" ) ] ]
                [ H.input
                    [ HA.id (toString o)
                    , HA.type_ "radio"
                    , HA.checked (isCurrent o)
                    , HA.style [ ( "margin-right", "10px" ) ]
                    , HE.onCheck <| always (ChangeData o)
                    ]
                    []
                , H.label [ HA.for (toString o) ] [ H.text (toString o) ]
                ]
    in
        H.div
            []
            [ H.div [] [ H.text "Dataset:" ]
            , H.br [] []
            , opt Empty
            , opt Singleton
            , opt Nice
            ]


dataset : Model -> LC.Dataset
dataset model =
    case model of
        Empty ->
            []

        Singleton ->
            [ [ ( 1, 3 ) ] ]

        Nice ->
            [ [ ( 100000, 3 ), ( 100001, 4 ), ( 100002, 3 ), ( 100003, 2 ), ( 100004, 1 ), ( 100005, 1 ), ( 100006, -1 ) ]
            , [ ( 100000, 1 ), ( 100001, 2.5 ), ( 100002, 3 ), ( 100003, 3.5 ), ( 100004, 3 ), ( 100005, 2 ), ( 100006, 0 ) ]
            , [ ( 100000, 2 ), ( 100001, 1.5 ), ( 100002, 0 ), ( 100003, 3 ), ( 100004, -0.5 ), ( 100005, -1.5 ), ( 100006, -2 ) ]
            ]
