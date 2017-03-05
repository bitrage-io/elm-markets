module Markets.Bitfinex.TradeHistory exposing (..)

import Erl
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)
import Json.Decode.Pipeline exposing (..)
import Model.MarketName exposing (MarketName(Bitfinex))
import Model.Trade as Trade exposing (Trade)
import Model.Pair as Pair exposing (Pair)
import Markets.Bitfinex.Util as Util
import Time exposing (Time)


type alias Options =
    { pair : Pair
    , time : Maybe Time
    , limit : Int
    }


url : Options -> String
url options =
    let
        base =
            ("https://api.bitfinex.com/v1/trades/" ++ (Util.pairToString options.pair))
                |> Erl.parse

        baseWithTime =
            case options.time of
                Just time ->
                    base
                        |> Erl.setQuery "timestamp" (String.toLower <| toString time)

                Nothing ->
                    base
    in
        baseWithTime
            |> Erl.setQuery "limit_trades" (toString options.limit)
            |> Erl.toString


decoder : Pair -> Decoder (List Trade)
decoder pair =
    customDecoder
        (list <| maybe <| tradeDecoder pair)
        (Ok << List.filterMap identity)


tradeDecoder : Pair -> Decoder Trade
tradeDecoder pair =
    decode Trade
        |> required "tid" (customDecoder float (Ok << toString))
        |> hardcoded Bitfinex
        |> hardcoded pair
        |> required "timestamp" timestamp
        |> required "type" Trade.sideDecoder
        |> required "price" string
        |> required "amount" string
