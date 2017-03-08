module Market.Api.Poloniex.OrderBooks exposing (..)

import Dict exposing (Dict)
import Erl
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)
import Json.Decode.Pipeline exposing (..)
import Market
import Market.Api.Poloniex.Util as Util
import Market.Decode


type alias Options =
    { pair : Maybe Market.Pair
    , depth : Int
    }


url : Options -> String
url options =
    let
        currencyPair =
            case options.pair of
                Just pair ->
                    Util.pairToString pair

                Nothing ->
                    "all"
    in
        "https://poloniex.com/public"
            |> Erl.parse
            |> Erl.setQuery "command" "returnOrderBook"
            |> Erl.setQuery "currencyPair" currencyPair
            |> Erl.setQuery "depth" (toString options.depth)
            |> Erl.toString


decoder : Maybe Market.Pair -> Decoder (List Market.OrderBook)
decoder maybePair =
    case maybePair of
        Just pair ->
            oneOf
                [ singleDecoder pair |> andThen (\o -> succeed [ o ])
                , multipleDecoder
                ]

        Nothing ->
            multipleDecoder


singleDecoder : Market.Pair -> Decoder Market.OrderBook
singleDecoder pair =
    decode Market.OrderBook
        |> hardcoded Market.Poloniex
        |> hardcoded pair
        |> hardcoded Market.emptyOrder
        |> required "asks" (list orderDecoder)
        |> hardcoded Market.emptyOrder
        |> required "bids" (list orderDecoder)
        |> andThen Market.Decode.orderBookBestPrices


multipleDecoder : Decoder (List Market.OrderBook)
multipleDecoder =
    customDecoder (dict <| singleDecoder <| Market.emptyPair) fromDict


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


orderDecoder : Decoder Market.Order
orderDecoder =
    decode Market.Order
        |> custom (index 0 string)
        |> custom (index 0 anyFloat)
        |> custom (index 1 (asString float toString))
        |> custom (index 1 anyFloat)
