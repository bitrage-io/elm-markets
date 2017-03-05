module Json.Encode.Extra exposing (..)

import Date
import Dict
import Json.Encode as E
import Date.Extra.Format exposing (utcIsoString)


maybe : (a -> E.Value) -> Maybe a -> E.Value
maybe toValue maybe =
    case maybe of
        Just x ->
            toValue x

        Nothing ->
            E.null


date : Date.Date -> E.Value
date d =
    d
        |> utcIsoString
        |> E.string


dict : (a -> E.Value) -> Dict.Dict String a -> E.Value
dict encodeValue userDict =
    let
        dictList =
            Dict.toList userDict

        encodedList =
            List.map (\( k, v ) -> ( k, encodeValue v )) dictList
    in
        E.object encodedList
