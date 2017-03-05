module Markets.Kraken.OrderBook exposing (..)

import Dict exposing (Dict)
import Erl
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)
import Json.Decode.Pipeline exposing (..)
import Model.MarketName exposing (MarketName(Kraken))
import Model.Order as Order
import Model.OrderBook as OrderBook exposing (OrderBook)
import Model.Pair as Pair exposing (Pair)
import Markets.Kraken.Util as Util


type alias Options =
    { pair : Pair
    , count : Int
    }


url : Options -> String
url options =
    "https://api.kraken.com/0/public/Depth"
        |> Erl.parse
        |> Erl.setQuery "pair" (Util.pairToString options.pair)
        |> Erl.setQuery "count" (toString <| abs options.count)
        |> Erl.toString


decoder : Pair -> Decoder OrderBook
decoder pair =
    customDecoder (field "result" <| dict (orderBookDecoder pair))
        (\d ->
            d
                |> Dict.toList
                |> List.head
                |> Maybe.andThen (\( _, r ) -> Just { r | pair = pair })
                |> Result.fromMaybe "Empty order book"
        )


orderBookDecoder : Pair -> Decoder OrderBook
orderBookDecoder pair =
    decode OrderBook
        |> hardcoded Kraken
        |> hardcoded pair
        |> required "asks" (list orderDecoder)
        |> required "bids" (list orderDecoder)


orderDecoder : Decoder Order.Order
orderDecoder =
    map2 Order.Order
        (index 0 string)
        (index 1 string)
