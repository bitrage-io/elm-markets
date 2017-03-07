module Market.Api.Poloniex.OrderBooks exposing (..)

import Dict exposing (Dict)
import Erl
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)
import Json.Decode.Pipeline exposing (..)
import Market exposing (..)
import Market.Api.Poloniex.Util as Util


type alias Options =
    { pair : Maybe Pair
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


decoder : Maybe Pair -> Decoder (List OrderBook)
decoder maybePair =
    case maybePair of
        Just pair ->
            oneOf
                [ singleDecoder pair |> andThen (\orderBook -> succeed [ orderBook ])
                , multipleDecoder
                ]

        Nothing ->
            multipleDecoder


singleDecoder : Pair -> Decoder OrderBook
singleDecoder pair =
    decode OrderBook
        |> hardcoded Poloniex
        |> hardcoded pair
        |> required "asks" (list orderDecoder)
        |> required "bids" (list orderDecoder)


multipleDecoder : Decoder (List OrderBook)
multipleDecoder =
    customDecoder (dict <| singleDecoder <| emptyPair) fromDict


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


orderDecoder : Decoder Market.Order
orderDecoder =
    map2 Market.Order
        (index 0 string)
        (index 1 (asString float toString))
