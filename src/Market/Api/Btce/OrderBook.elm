module Markets.Btce.OrderBook exposing (..)

import Dict exposing (Dict)
import Erl
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)
import Json.Decode.Pipeline exposing (..)
import Model.MarketName exposing (MarketName(Btce))
import Model.Order as Order
import Model.OrderBook as OrderBook exposing (OrderBook)
import Model.Pair as Pair exposing (Pair)
import Model.Symbol as Symbol exposing (Symbol)
import Markets.Btce.Util as Util


type alias Options =
    { pairs : List Pair
    }


url : Options -> String
url options =
    ("https://btc-e.com/api/3/depth/" ++ Util.joinPairs options.pairs)
        |> Erl.parse
        |> Erl.setQuery "ignore_invalid" "1"
        |> Erl.toString


decoder : Decoder (List OrderBook)
decoder =
    customDecoder (dict <| orderBookDecoder <| Pair Symbol.Unset Symbol.Unset) fromDict


fromDict : Dict String OrderBook -> Result String (List OrderBook)
fromDict orderBooks =
    orderBooks
        |> Dict.toList
        |> List.filterMap withPair
        |> Ok


withPair : ( String, OrderBook ) -> Maybe OrderBook
withPair ( pairStr, orderBook ) =
    case Util.pairFromString pairStr of
        Ok pair ->
            Just { orderBook | pair = pair }

        Err _ ->
            Nothing


orderBookDecoder : Pair -> Decoder OrderBook
orderBookDecoder pair =
    decode OrderBook
        |> hardcoded Btce
        |> hardcoded pair
        |> required "asks" (list orderDecoder)
        |> required "bids" (list orderDecoder)


orderDecoder : Decoder Order.Order
orderDecoder =
    map2 Order.Order
        (index 0 <| asString float toString)
        (index 1 <| asString float toString)
