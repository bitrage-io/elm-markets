module Market.Decode
    exposing
        ( market
        , symbol
        , pair
        , side
        , order
        , orderBook
        , trade
        )

{-|

@docs market, symbol, pair, side, order, orderBook, trade
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
        |> required "price" string
        |> required "volume" string


{-| Decodes an OrderBook
-}
orderBook : Decoder Market.OrderBook
orderBook =
    decode Market.OrderBook
        |> required "market" market
        |> required "pair" pair
        |> required "asks" (list order)
        |> required "bids" (list order)


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
        |> required "price" string
        |> required "volume" string
