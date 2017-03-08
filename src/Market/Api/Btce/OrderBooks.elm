module Market.Api.Btce.OrderBooks exposing (..)

import Dict exposing (Dict)
import Erl
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)
import Json.Decode.Pipeline exposing (..)
import Market
import Market.Api.Btce.Util as Util
import Market.Decode


type alias Options =
    { pairs : List Market.Pair
    }


url : Options -> String
url options =
    ("https://btc-e.com/api/3/depth/" ++ Util.joinPairs options.pairs)
        |> Erl.parse
        |> Erl.setQuery "ignore_invalid" "1"
        |> Erl.toString


decoder : Decoder (List Market.OrderBook)
decoder =
    customDecoder (dict <| orderBookDecoder <| Market.emptyPair) fromDict


fromDict : Dict String Market.OrderBook -> Result String (List Market.OrderBook)
fromDict orderBooks =
    orderBooks
        |> Dict.toList
        |> List.filterMap withPair
        |> Ok


withPair : ( String, Market.OrderBook ) -> Maybe Market.OrderBook
withPair ( pairStr, orderBook ) =
    case Util.pairFromString pairStr of
        Ok pair ->
            Just { orderBook | pair = pair }

        Err _ ->
            Nothing


orderBookDecoder : Market.Pair -> Decoder Market.OrderBook
orderBookDecoder pair =
    decode Market.OrderBook
        |> hardcoded Market.Btce
        |> hardcoded pair
        |> hardcoded Market.emptyOrder
        |> required "asks" (list orderDecoder)
        |> hardcoded Market.emptyOrder
        |> required "bids" (list orderDecoder)
        |> andThen Market.Decode.orderBookBestPrices


orderDecoder : Decoder Market.Order
orderDecoder =
    decode Market.Order
        |> custom (index 0 <| asString float toString)
        |> custom (index 0 anyFloat)
        |> custom (index 1 <| asString float toString)
        |> custom (index 1 anyFloat)
