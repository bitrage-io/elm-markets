module Markets.Kraken.TradeHistory exposing (..)

import Markets.Kraken.Util as Util
import Dict exposing (Dict)
import Erl
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)
import Json.Decode.Pipeline exposing (..)
import Model.MarketName exposing (MarketName(Kraken))
import Model.Pair as Pair exposing (Pair)
import Model.Trade as Trade exposing (Trade)
import Time exposing (Time)


type alias Options =
    { pair : Pair
    , time : Maybe Time
    }


url : Options -> String
url options =
    let
        base =
            "https://api.kraken.com/0/public/Trades"
                |> Erl.parse
                |> Erl.setQuery "pair" (Util.pairToString options.pair)

        baseWithTime =
            case options.time of
                Just time ->
                    Erl.setQuery "time" (toString time) base

                Nothing ->
                    base
    in
        Erl.toString baseWithTime


decoder : Pair -> Decoder (List Trade)
decoder pair =
    customDecoder (field "result" <| dict <| maybe <| list <| tradeDecoder pair)
        (\d ->
            d
                |> Dict.toList
                |> List.head
                |> Maybe.andThen Tuple.second
                |> Maybe.andThen (Just << List.map Trade.generateId)
                |> Result.fromMaybe "Empty trade history"
        )


tradeDecoder : Pair -> Decoder Trade
tradeDecoder pair =
    decode Trade
        |> hardcoded "EMPTY ID"
        |> hardcoded Kraken
        |> hardcoded pair
        |> custom (index 2 timestamp)
        |> custom (index 3 Trade.sideDecoder)
        |> custom (index 0 string)
        |> custom (index 1 string)
