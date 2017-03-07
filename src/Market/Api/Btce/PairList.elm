module Markets.Btce.PairList exposing (..)

import Markets.Btce.Util as Util
import Dict
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)
import Model.Pair as Pair exposing (Pair)


url : String
url =
    "https://btc-e.com/api/3/info"


decoder : Decoder (List Pair)
decoder =
    customDecoder (field "pairs" (dict <| succeed ()))
        (\d ->
            d
                |> Dict.toList
                |> List.map Tuple.first
                |> List.filterMap (Result.toMaybe << Util.pairFromString)
                |> Ok
        )
