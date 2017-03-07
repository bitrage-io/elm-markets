module Markets.Bitfinex.OrderBook exposing (..)

import Erl
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Model.MarketName exposing (MarketName(Bitfinex))
import Model.Order as Order
import Model.OrderBook as OrderBook exposing (OrderBook)
import Model.Pair as Pair exposing (Pair)
import Markets.Bitfinex.Util as Util


type alias Options =
    { pair : Pair
    , depth : Int
    , group : Bool
    }


url : Options -> String
url options =
    ("https://api.bitfinex.com/v1/book/" ++ (Util.pairToString options.pair))
        |> Erl.parse
        |> Erl.setQuery "limit_asks" (toString <| abs options.depth)
        |> Erl.setQuery "limit_bids" (toString <| abs options.depth)
        |> Erl.setQuery "group"
            (if options.group then
                "1"
             else
                "0"
            )
        |> Erl.toString


decoder : Pair -> Decoder OrderBook
decoder pair =
    decode OrderBook
        |> hardcoded Bitfinex
        |> hardcoded pair
        |> required "asks" (list orderDecoder)
        |> required "bids" (list orderDecoder)


orderDecoder : Decoder Order.Order
orderDecoder =
    map2 Order.Order
        (field "price" string)
        (field "amount" string)
