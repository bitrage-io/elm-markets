module Market.Decode
    exposing
        ( marketName
        , symbol
        , pair
        , side
        , order
        , orderBook
        , trade
        )

{-|

@docs marketName, symbol, pair, side, order, orderBook, trade
-}

import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)
import Json.Decode.Pipeline exposing (..)
import Market exposing (..)


{-| Decodes a Market
-}
marketName : Decoder MarketName
marketName =
    customDecoder string marketNameFromString


{-| Decodes a Symbol
-}
symbol : Decoder Symbol
symbol =
    customDecoder string symbolFromString


{-| Decodes a Pair
-}
pair : Decoder Pair
pair =
    customDecoder string pairFromString


{-| Decodes a Side
-}
side : Decoder Side
side =
    customDecoder string sideFromString


{-| Decodes an Order
-}
order : Decoder Market.Order
order =
    decode Order
        |> required "price" string
        |> required "volume" string


{-| Decodes an OrderBook
-}
orderBook : Decoder OrderBook
orderBook =
    decode OrderBook
        |> required "marketName" marketName
        |> required "pair" pair
        |> required "asks" (list order)
        |> required "bids" (list order)


{-| Decodes a Trade
-}
trade : Decoder Trade
trade =
    decode Trade
        |> optional "id" (maybe string) Nothing
        |> required "marketName" marketName
        |> required "pair" pair
        |> required "date" date
        |> required "side" side
        |> required "price" string
        |> required "volume" string
