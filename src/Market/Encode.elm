module Market.Encode
    exposing
        ( marketName
        , symbol
        , pair
        , side
        , order
        , orderBook
        , trade
        , error
        )

{-|

@docs marketName, symbol, pair, side, order, orderBook, trade, error
-}

import Json.Encode exposing (..)
import Json.Encode.Extra exposing (..)
import Market exposing (..)
import Http


{-| Encodes a Market
-}
marketName : MarketName -> Value
marketName =
    string << marketNameToString


{-| Encodes a Symbol
-}
symbol : Symbol -> Value
symbol =
    string << symbolToString


{-| Encodes a Pair
-}
pair : Pair -> Value
pair =
    string << pairToString


{-| Encodes a Side
-}
side : Side -> Value
side =
    string << sideToString


{-| Encodes an Order
-}
order : Market.Order -> Value
order o =
    object
        [ ( "price", string o.price )
        , ( "volume", string o.volume )
        ]


{-| Encodes an OrderBook
-}
orderBook : OrderBook -> Value
orderBook o =
    object
        [ ( "market_name", marketName o.marketName )
        , ( "pair", pair o.pair )
        , ( "asks", list <| List.map order o.asks )
        , ( "bids", list <| List.map order o.bids )
        ]


{-| Encodes a Trade
-}
trade : Trade -> Value
trade t =
    object
        [ ( "id", maybe string t.id )
        , ( "market_name", marketName t.marketName )
        , ( "pair", pair t.pair )
        , ( "date", date t.date )
        , ( "side", side t.side )
        , ( "price", string t.price )
        , ( "volume", string t.volume )
        ]


{-| Encodes an ApiError
-}
error : Error -> Value
error e =
    case e of
        Http.BadUrl url ->
            object
                [ ( "reason", string "BadUrl" )
                , ( "url", string url )
                ]

        Http.Timeout ->
            object [ ( "reason", string "Timeout" ) ]

        Http.NetworkError ->
            object [ ( "reason", string "NetworkError" ) ]

        Http.BadStatus response ->
            object
                [ ( "reason", string "BadStatus" )
                , ( "url", string response.url )
                , ( "status_code", int response.status.code )
                , ( "status_message", string response.status.message )
                , ( "body", string response.body )
                ]

        Http.BadPayload message response ->
            object
                [ ( "reason", string "BadPayload" )
                , ( "url", string response.url )
                , ( "error_message", string message )
                , ( "status_code", int response.status.code )
                , ( "status_message", string response.status.message )
                , ( "body", string response.body )
                ]


response : Response -> Value
response r =
    case r of
        PairsResponse marketName_ pairs ->
            object
                [ ( "market_name", marketName marketName_ )
                , ( "pairs", list <| List.map pair pairs )
                ]

        OrderBooksResponse marketName_ orderBooks ->
            object
                [ ( "market_name", marketName marketName_ )
                , ( "orderBooks", list <| List.map orderBook orderBooks )
                ]

        TradesResponse marketName_ trades ->
            object
                [ ( "market_name", marketName marketName_ )
                , ( "trades", list <| List.map trade trades )
                ]

        ErrorResponse marketName_ error_ ->
            object
                [ ( "market_name", marketName marketName_ )
                , ( "error", error error_ )
                ]
