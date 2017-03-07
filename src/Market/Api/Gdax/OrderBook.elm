module Markets.Gdax.OrderBook exposing (..)

import Erl
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Model.MarketName exposing (MarketName(Gdax))
import Model.Order as Order
import Model.OrderBook as OrderBook exposing (OrderBook)
import Model.Pair as Pair exposing (Pair)
import Markets.Gdax.Util as Util


type alias Options =
    { pair : Pair
    , level : Level
    }


type Level
    = BestOnly
    | Top50
    | FullBook


levelToInt : Level -> Int
levelToInt level =
    case level of
        BestOnly ->
            1

        Top50 ->
            2

        FullBook ->
            3


url : Options -> String
url options =
    ("https://api.gdax.com/products/" ++ (Util.pairToString options.pair) ++ "/book")
        |> Erl.parse
        |> Erl.setQuery "level" (toString <| levelToInt options.level)
        |> Erl.toString


decoder : Pair -> Decoder OrderBook
decoder pair =
    decode OrderBook
        |> hardcoded Gdax
        |> hardcoded pair
        |> required "asks" (list orderDecoder)
        |> required "bids" (list orderDecoder)


orderDecoder : Decoder Order.Order
orderDecoder =
    map2 Order.Order
        (index 0 string)
        (index 1 string)
