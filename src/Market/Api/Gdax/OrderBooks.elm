module Market.Api.Gdax.OrderBooks exposing (..)

import Erl
import Http
import Json.Decode as Decode
import Json.Decode.Extra as Decode
import Json.Decode.Pipeline as Decode
import Market
import Market.Api
import Market.Api.Gdax.Util as Gdax
import Market.Decode


orderBooks : Market.Api.OrderBooksOptions -> List (Http.Request (List Market.OrderBook))
orderBooks { pairs, depth } =
    List.map (orderBook depth) pairs


orderBook : Int -> Market.Pair -> Http.Request (List Market.OrderBook)
orderBook depth pair =
    Http.get (url depth pair) (decoder pair)


type Level
    = BestOnly
    | Top50
    | FullBook


levelFromDepth : Int -> Level
levelFromDepth depth =
    if depth > 50 then
        FullBook
    else if depth == 1 then
        BestOnly
    else
        Top50


levelToInt : Level -> Int
levelToInt level =
    case level of
        BestOnly ->
            1

        Top50 ->
            2

        FullBook ->
            3


url : Int -> Market.Pair -> String
url depth pair =
    ("https://api.gdax.com/products/" ++ (Gdax.pairToString pair) ++ "/book")
        |> Erl.parse
        |> Erl.setQuery "level" (toString <| levelToInt <| levelFromDepth depth)
        |> Erl.toString


decoder : Market.Pair -> Decode.Decoder (List Market.OrderBook)
decoder pair =
    Decode.decode Market.OrderBook
        |> Decode.hardcoded Market.Gdax
        |> Decode.hardcoded pair
        |> Decode.hardcoded Market.emptyOrder
        |> Decode.required "asks" (Decode.list orderDecoder)
        |> Decode.hardcoded Market.emptyOrder
        |> Decode.required "bids" (Decode.list orderDecoder)
        |> Decode.andThen Market.Decode.orderBookBestPrices
        |> Decode.map (\orderBook -> [ orderBook ])


orderDecoder : Decode.Decoder Market.Order
orderDecoder =
    Decode.decode Market.Order
        |> Decode.custom (Decode.index 0 Decode.string)
        |> Decode.custom (Decode.index 0 Decode.anyFloat)
        |> Decode.custom (Decode.index 1 Decode.string)
        |> Decode.custom (Decode.index 1 Decode.anyFloat)
