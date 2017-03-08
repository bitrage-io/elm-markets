module Market.Api.Gdax.Pairs exposing (pairs)

import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Market
import Market.Decode


pairs : Http.Request (List Market.Pair)
pairs =
    Http.get url decoder


url : String
url =
    "https://api.gdax.com/products"


decoder : Decode.Decoder (List Market.Pair)
decoder =
    Decode.list pairDecoder


pairDecoder : Decode.Decoder Market.Pair
pairDecoder =
    Decode.decode Market.Pair
        |> Decode.required "base_currency" Market.Decode.symbol
        |> Decode.required "quote_currency" Market.Decode.symbol
