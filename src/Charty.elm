module Charty
    exposing
        ( DataPoint
        , SvgPoint
        , Transform
        , calculateTransform
        )


type alias DataPoint =
    ( Float, Float )


type alias SvgPoint =
    ( Float, Float )


type alias Transform =
    DataPoint -> SvgPoint


calculateTransform : List DataPoint -> Transform
calculateTransform points =
    let
        xMax =
            points
                |> List.map (\( x, y ) -> x)
                |> List.maximum

        yMax =
            points
                |> List.map (\( x, y ) -> y)
                |> List.maximum
    in
        \( x, y ) ->
            case ( xMax, yMax ) of
                ( Just xM, Just yM ) ->
                    ( 50 + (x * 900 / xM), 950 - (y * 900 / yM) )

                _ ->
                    ( x, y )
