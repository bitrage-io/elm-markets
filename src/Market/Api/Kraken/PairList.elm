module Markets.Kraken.PairList exposing (..)

import Dict
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)
import Model.Pair as Pair exposing (Pair)
import Markets.Kraken.Util as Util


url : String
url =
    "https://api.kraken.com/0/public/AssetPairs"


decoder : Decoder (List Pair)
decoder =
    customDecoder (field "result" <| dict pairDecoder)
        (\d ->
            d
                |> Dict.toList
                |> List.filterMap Tuple.second
                |> Ok
        )


pairDecoder : Decoder (Maybe Pair)
pairDecoder =
    customDecoder (field "altname" string)
        (\str ->
            if String.endsWith ".d" str then
                Ok Nothing
            else
                Result.map Just <| Util.pairFromString str
        )
