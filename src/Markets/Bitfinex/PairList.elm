module Markets.Bitfinex.PairList exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)
import Model.Pair as Pair exposing (Pair)
import Markets.Bitfinex.Util as Util


url : String
url =
    "https://api.bitfinex.com/v1/symbols"


decoder : Decoder (List Pair)
decoder =
    list pairDecoder


pairDecoder : Decoder Pair
pairDecoder =
    customDecoder string Util.pairFromString
