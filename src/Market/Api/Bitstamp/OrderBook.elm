module Markets.Bitstamp.OrderBook exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)
import Json.Decode.Pipeline exposing (..)
import Model.MarketName exposing (MarketName(Bitstamp))
import Model.Order as Order
import Model.OrderBook as OrderBook exposing (OrderBook)
import Model.Pair as Pair exposing (Pair)
import Markets.Bitstamp.Util as Util


type alias Options =
    { pair : Pair
    }


url : Options -> String
url options =
    "https://www.bitstamp.net/api/v2/order_book/" ++ (Util.pairToString options.pair)


decoder : Pair -> Decoder OrderBook
decoder pair =
    decode OrderBook
        |> hardcoded Bitstamp
        |> hardcoded pair
        |> required "asks" (list orderDecoder)
        |> required "bids" (list orderDecoder)


orderDecoder : Decoder Order.Order
orderDecoder =
    map2 Order.Order
        (index 0 string)
        (index 1 string)
