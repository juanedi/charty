module Charty.ArrayUtil
    exposing
        ( unsafeGet
        , unsafeFirst
        , unsafeLast
        , maximum
        , unsafeMaximum
        )

import Array


{-
   The purpose of these "unsafe" functions is to avoid the boilerplate of
   extracting results from Maybe's when we know were are not in the empty case.

   That means we're trading convenience for the risk of runtime errors. This may be
   a sign that the wrong data structure is being used.
-}


unsafeGet i array =
    case Array.get i array of
        Nothing ->
            Debug.crash "invalid state"

        Just x ->
            x


unsafeFirst array =
    unsafeGet 0 array


unsafeLast array =
    unsafeGet (Array.length array - 1) array


maximum =
    let
        combine x r =
            Maybe.withDefault x r |> Basics.max x |> Just
    in
        Array.foldr combine Nothing


unsafeMaximum a =
    case maximum a of
        Nothing ->
            Debug.crash "invalid state"

        Just x ->
            x
