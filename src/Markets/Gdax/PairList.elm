module Markets.Gdax.PairList exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Model.Pair as Pair exposing (Pair)
import Model.Symbol as Symbol


url : String
url =
    "https://api.gdax.com/products"


decoder : Decoder (List Pair)
decoder =
    list pairDecoder


pairDecoder : Decoder Pair
pairDecoder =
    decode Pair
        |> required "base_currency" Symbol.decoder
        |> required "quote_currency" Symbol.decoder
