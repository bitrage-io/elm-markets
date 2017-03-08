module Market.Api.Btce.Pairs exposing (..)

import Dict
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)
import Market
import Market.Api.Btce.Util as Util


url : String
url =
    "https://btc-e.com/api/3/info"


decoder : Decoder (List Market.Pair)
decoder =
    customDecoder (field "pairs" (dict <| succeed ()))
        (\d ->
            d
                |> Dict.toList
                |> List.map Tuple.first
                |> List.filterMap (Result.toMaybe << Util.pairFromString)
                |> Ok
        )
