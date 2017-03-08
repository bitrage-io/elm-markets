module Market.Api.Btce.Trades exposing (..)

import Dict exposing (Dict)
import Erl
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)
import Json.Decode.Pipeline exposing (..)
import Market
import Market.Api.Btce.Util as Util
import Market.Decode


type alias Options =
    { pairs : List Market.Pair
    }


url : Options -> String
url options =
    ("https://btc-e.com/api/3/trades/" ++ Util.joinPairs options.pairs)
        |> Erl.parse
        |> Erl.setQuery "ignore_invalid" "1"
        |> Erl.toString


decoder : Decoder (List Market.Trade)
decoder =
    customDecoder (dict (list tradeDecoder)) fromDict


fromDict : Dict String (List Market.Trade) -> Result String (List Market.Trade)
fromDict orderBooks =
    orderBooks
        |> Dict.toList
        |> List.filterMap withPair
        |> List.concat
        |> Ok


withPair : ( String, List Market.Trade ) -> Maybe (List Market.Trade)
withPair ( pairStr, trades ) =
    case Util.pairFromString pairStr of
        Ok pair ->
            trades
                |> List.map (\t -> { t | pair = pair })
                |> Just

        Err _ ->
            Nothing


tradeDecoder : Decoder Market.Trade
tradeDecoder =
    decode Market.Trade
        |> required "tid" (maybe <| customDecoder float (Ok << toString))
        |> hardcoded Market.Btce
        |> hardcoded Market.emptyPair
        |> required "timestamp" timestamp
        |> required "type" Market.Decode.side
        |> required "price" (asString float toString)
        |> required "price" anyFloat
        |> required "amount" (asString float toString)
        |> required "amount" anyFloat
