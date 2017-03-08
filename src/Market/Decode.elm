module Market.Decode
    exposing
        ( market
        , symbol
        , pair
        , side
        , order
        , orderBook
        , orderBookBestPrices
        , trade
        )

{-|

@docs market, symbol, pair, side, order, orderBook, orderBookBestPrices, trade
-}

import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)
import Json.Decode.Pipeline exposing (..)
import Market


{-| Decodes a Market
-}
market : Decoder Market.Market
market =
    customDecoder string Market.marketFromString


{-| Decodes a Symbol
-}
symbol : Decoder Market.Symbol
symbol =
    customDecoder string Market.symbolFromString


{-| Decodes a Pair
-}
pair : Decoder Market.Pair
pair =
    customDecoder string Market.pairFromString


{-| Decodes a Side
-}
side : Decoder Market.Side
side =
    customDecoder string Market.sideFromString


{-| Decodes an Order
-}
order : Decoder Market.Order
order =
    decode Market.Order
        |> required "price_str" string
        |> required "price" float
        |> required "volume_str" string
        |> required "volume" float


{-| Decodes an OrderBook
-}
orderBook : Decoder Market.OrderBook
orderBook =
    decode Market.OrderBook
        |> required "market" market
        |> required "pair" pair
        |> required "lowest_ask" order
        |> required "other_asks" (list order)
        |> required "highest_bid" order
        |> required "other_bids" (list order)


{-| -}
orderBookBestPrices : Market.OrderBook -> Decoder Market.OrderBook
orderBookBestPrices orderBook =
    let
        sortedAsks =
            List.sortBy .price orderBook.otherAsks

        maybeLowestAsk =
            List.head sortedAsks

        otherAsks =
            List.tail sortedAsks |> Maybe.withDefault []

        sortedBids =
            List.sortBy .price orderBook.otherBids |> List.reverse

        maybeHighestBid =
            List.head sortedBids

        otherBids =
            List.tail sortedBids |> Maybe.withDefault []
    in
        case ( maybeLowestAsk, maybeHighestBid ) of
            ( Just lowestAsk, Just highestBid ) ->
                succeed
                    { orderBook
                        | lowestAsk = lowestAsk
                        , otherAsks = otherAsks
                        , highestBid = highestBid
                        , otherBids = otherBids
                    }

            ( Just _, Nothing ) ->
                fail "OrderBook had no lowest ask"

            ( Nothing, Just _ ) ->
                fail "OrderBook had no highest bid"

            ( Nothing, Nothing ) ->
                fail "OrderBook had no orders"


{-| Decodes a Trade
-}
trade : Decoder Market.Trade
trade =
    decode Market.Trade
        |> optional "id" (maybe string) Nothing
        |> required "market" market
        |> required "pair" pair
        |> required "date" date
        |> required "side" side
        |> required "price_str" string
        |> required "price" float
        |> required "volume_str" string
        |> required "volume" float
