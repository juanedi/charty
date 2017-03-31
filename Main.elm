module Main exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events as Events


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


view : Model -> Html Msg
view model =
    Html.div
        [ Html.Attributes.style [ ( "width", "100%" ), ( "height", "100%" ) ] ]
        [ Html.div
            [ Html.Attributes.style [ ( "height", "70vh" ), ( "margin", "0 auto" ) ] ]
            [ lineChart
                [ [ ( 150, 400 ), ( 350, 100 ), ( 550, 300 ), ( 850, 200 ) ]
                , [ ( 150, 800 ), ( 350, 500 ), ( 550, 700 ), ( 850, 600 ) ]
                ]
            ]
        , Html.div [] [ Html.text (Maybe.withDefault "Boo!" model) ]
        ]


lineChart : List (List ( Int, Int )) -> Svg Msg
lineChart series =
    svg
        [ width "100%", height "100%", viewBox "0 0 1000 1000" ]
        [ background
        , axis
        , g [] (List.map drawSeries series)
        ]


background : Svg Msg
background =
    rect [ width "1000", height "1000", fill "#FAFAFA" ] []


axis : Svg msg
axis =
    g []
        [ line [ x1 "50", y1 "50", x2 "50", y2 "950", stroke "#CACACA", Svg.Attributes.strokeDasharray "5 5" ] []
        , line [ x1 "50", y1 "950", x2 "950", y2 "950", stroke "#CACACA", Svg.Attributes.strokeDasharray "5 5" ] []
        ]


drawSeries : List ( Int, Int ) -> Svg Msg
drawSeries linePoints =
    g []
        [ drawLine linePoints
        , drawPoints linePoints
        ]


drawLine : List ( Int, Int ) -> Svg Msg
drawLine linePoints =
    let
        attr =
            linePoints
                |> List.map (\( x, y ) -> toString x ++ " " ++ toString y)
                |> String.join ", "
    in
        polyline [ points attr, stroke "blue", fill "transparent" ] []


drawPoints : List ( Int, Int ) -> Svg Msg
drawPoints linePoints =
    let
        drawPoint ( x, y ) =
            circle
                [ cx (toString x)
                , cy (toString y)
                , r "10"
                , fill "red"
                , Events.onMouseOver (SetMsg "hey!")
                , Events.onMouseOut ClearMsg
                ]
                []
    in
        g [] <|
            List.map drawPoint linePoints
