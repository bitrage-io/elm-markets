module Markets.Bitstamp.TradeHistory exposing (..)

import Erl
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)
import Json.Decode.Pipeline exposing (..)
import Model.MarketName exposing (MarketName(Bitstamp))
import Model.Trade as Trade exposing (Trade)
import Model.Pair as Pair exposing (Pair)
import Markets.Bitstamp.Util as Util


type alias Options =
    { pair : Pair
    , time : Time
    }


type Time
    = Minute
    | Hour
    | Day


url : Options -> String
url options =
    ("https://www.bitstamp.net/api/v2/transactions/" ++ (Util.pairToString options.pair))
        |> Erl.parse
        |> Erl.setQuery "time" (String.toLower <| toString options.time)
        |> Erl.toString


decoder : Pair -> Decoder (List Trade)
decoder pair =
    list (tradeDecoder pair)


tradeDecoder : Pair -> Decoder Trade
tradeDecoder pair =
    decode Trade
        |> required "tid" string
        |> hardcoded Bitstamp
        |> hardcoded pair
        |> required "date" timestamp
        |> required "type" sideDecoder
        |> required "price" string
        |> required "amount" string


sideDecoder : Decoder Trade.Side
sideDecoder =
    customDecoder string
        (\s ->
            if s == "0" then
                Ok Trade.Buy
            else if s == "1" then
                Ok Trade.Sell
            else
                Err <| "'" ++ s ++ "' is not a known side"
        )
