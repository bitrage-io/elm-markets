module Market.Api.Gdax.Trades exposing (..)

import Erl
import Http
import Json.Decode as Decode
import Json.Decode.Extra as Decode
import Json.Decode.Pipeline as Decode
import Market
import Market.Api
import Market.Api.Gdax.Util as Gdax
import Market.Decode


trades : Market.Api.TradesOptions -> List (Http.Request (List Market.Trade))
trades { pairs } =
    List.map tradesForPair pairs


tradesForPair : Market.Pair -> Http.Request (List Market.Trade)
tradesForPair pair =
    Http.get (url 0 pair) (decoder pair)


url : Int -> Market.Pair -> String
url page pair =
    ("https://api.gdax.com/products/" ++ (Gdax.pairToString pair) ++ "/trades")
        |> Erl.parse
        |> Erl.setQuery "p" (toString page)
        |> Erl.toString


decoder : Market.Pair -> Decode.Decoder (List Market.Trade)
decoder pair =
    Decode.list (tradeDecoder pair)


tradeDecoder : Market.Pair -> Decode.Decoder Market.Trade
tradeDecoder pair =
    Decode.decode Market.Trade
        |> Decode.required "trade_id" (Decode.maybe <| Decode.customDecoder Decode.float (Ok << toString))
        |> Decode.hardcoded Market.Gdax
        |> Decode.hardcoded pair
        |> Decode.required "time" Decode.date
        |> Decode.required "side" Market.Decode.side
        |> Decode.required "price" Decode.string
        |> Decode.required "price" Decode.anyFloat
        |> Decode.required "size" Decode.string
        |> Decode.required "size" Decode.anyFloat
