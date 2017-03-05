module Markets.Gdax.TradeHistory exposing (..)

import Erl
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)
import Json.Decode.Pipeline exposing (..)
import Model.MarketName exposing (MarketName(Gdax))
import Model.Trade as Trade exposing (Trade)
import Model.Pair as Pair exposing (Pair)
import Markets.Gdax.Util as Util


type alias Options =
    { pair : Pair
    , page : Int
    }


url : Options -> String
url options =
    ("https://api.gdax.com/products/" ++ (Util.pairToString options.pair) ++ "/trades")
        |> Erl.parse
        |> Erl.setQuery "p" (toString options.page)
        |> Erl.toString


decoder : Pair -> Decoder (List Trade)
decoder pair =
    list (tradeDecoder pair)


tradeDecoder : Pair -> Decoder Trade
tradeDecoder pair =
    decode Trade
        |> required "trade_id" (customDecoder float (Ok << toString))
        |> hardcoded Gdax
        |> hardcoded pair
        |> required "time" date
        |> required "side" Trade.sideDecoder
        |> required "price" string
        |> required "size" string
