module Markets.Btce.TradeHistory exposing (..)

import Dict exposing (Dict)
import Erl
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)
import Json.Decode.Pipeline exposing (..)
import Model.MarketName exposing (MarketName(Btce))
import Model.Pair as Pair exposing (Pair)
import Model.Symbol as Symbol exposing (Symbol(..))
import Model.Trade as Trade exposing (Trade)
import Markets.Btce.Util as Util


type alias Options =
    { pairs : List Pair
    }


url : Options -> String
url options =
    ("https://btc-e.com/api/3/trades/" ++ Util.joinPairs options.pairs)
        |> Erl.parse
        |> Erl.setQuery "ignore_invalid" "1"
        |> Erl.toString


decoder : Decoder (List Trade)
decoder =
    customDecoder (dict (list tradeDecoder)) fromDict


fromDict : Dict String (List Trade) -> Result String (List Trade)
fromDict orderBooks =
    orderBooks
        |> Dict.toList
        |> List.filterMap withPair
        |> List.concat
        |> Ok


withPair : ( String, List Trade ) -> Maybe (List Trade)
withPair ( pairStr, trades ) =
    case Util.pairFromString pairStr of
        Ok pair ->
            trades
                |> List.map (\t -> { t | pair = pair })
                |> Just

        Err _ ->
            Nothing


tradeDecoder : Decoder Trade
tradeDecoder =
    decode Trade
        |> required "tid" (customDecoder float (Ok << toString))
        |> hardcoded Btce
        |> hardcoded (Pair Unset Unset)
        |> required "timestamp" timestamp
        |> required "type" Trade.sideDecoder
        |> required "price" (asString float toString)
        |> required "amount" (asString float toString)
