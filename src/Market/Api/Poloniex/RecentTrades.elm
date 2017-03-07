module Market.Api.Poloniex.RecentTrades exposing (..)

import Date exposing (Date)
import Erl
import Json.Decode as D
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Market exposing (..)
import Market.Decode as D
import Market.Api.Poloniex.Util as Util
import Time exposing (Time)


type alias Options =
    { pair : Pair
    , start : Maybe Time
    , end : Maybe Time
    }


url : Options -> String
url options =
    let
        base =
            "https://poloniex.com/public"
                |> Erl.parse
                |> Erl.setQuery "command" "returnTradeHistory"
                |> Erl.setQuery "currencyPair" (Util.pairToString options.pair)

        baseWithStart =
            case options.start of
                Just start ->
                    base |> Erl.setQuery "start" (toString start)

                Nothing ->
                    base

        baseWithEnd =
            case options.end of
                Just end ->
                    base |> Erl.setQuery "end" (toString end)

                Nothing ->
                    base
    in
        baseWithEnd |> Erl.toString


decoder : Pair -> D.Decoder (List Trade)
decoder pair =
    D.list <| tradeDecoder pair


tradeDecoder : Pair -> D.Decoder Trade
tradeDecoder pair =
    D.decode Trade
        |> D.required "globalTradeID" (D.maybe <| D.customDecoder D.float (Ok << toString))
        |> D.hardcoded Poloniex
        |> D.hardcoded pair
        |> D.required "date" D.date
        |> D.required "type" D.side
        |> D.required "rate" D.string
        |> D.required "amount" D.string


{-| Format the date as UTC/ISO so it picks up on the timezone
-}
dateDecoder : D.Decoder Date
dateDecoder =
    D.customDecoder D.string
        (\dateTimeStr ->
            case String.split " " dateTimeStr of
                [ dateStr, timeStr ] ->
                    Date.fromString <| dateStr ++ "T" ++ timeStr ++ "Z"

                _ ->
                    Err <| "'" ++ dateTimeStr ++ "' is an unexpected date format"
        )
