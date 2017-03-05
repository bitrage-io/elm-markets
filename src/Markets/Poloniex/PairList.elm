module Markets.Poloniex.PairList exposing (..)

import Dict
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)
import Market exposing (Pair)
import Markets.Poloniex.Util as Util


url : String
url =
    "https://poloniex.com/public?command=returnTicker"


decoder : Decoder (List Pair)
decoder =
    customDecoder (dict (succeed ()))
        (\d ->
            d
                |> Dict.toList
                |> List.map Tuple.first
                |> toPairs
        )


toPairs : List String -> Result String (List Pair)
toPairs pairStrs =
    let
        pairResults =
            List.map Util.pairFromString pairStrs

        isErr result =
            case result of
                Ok _ ->
                    False

                Err _ ->
                    True

        firstErr =
            pairResults
                |> List.filter isErr
                |> List.head
    in
        case firstErr of
            Just (Err err) ->
                Err err

            _ ->
                Ok <| List.filterMap Result.toMaybe pairResults
