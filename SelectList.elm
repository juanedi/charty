module SelectList exposing (..)

import List


type alias Item a =
    Maybe a


select : List (Item a) -> List a
select =
    List.foldr
        (\item rec ->
            case item of
                Just x ->
                    x :: rec

                Nothing ->
                    rec
        )
        []


iff : Bool -> a -> Item a
iff include x =
    if include then
        Just x
    else
        Nothing


unless : Bool -> a -> Item a
unless exclude x =
    iff (not exclude) x


maybe : Maybe a -> Item a
maybe =
    identity


include : a -> Item a
include =
    Just
