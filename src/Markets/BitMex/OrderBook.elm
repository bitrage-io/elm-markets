module Markets.BitMex.OrderBook exposing (..)

import Erl
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)
import Json.Decode.Pipeline exposing (..)
import Model.MarketName exposing (MarketName(BitMex))
import Model.OrderBook as OrderBook exposing (OrderBook)
import Model.Pair as Pair exposing (Pair)
import Markets.BitMex.Util as Util


type alias Options =
    { pair : Pair
    , depth : Int
    }


url : Options -> String
url options =
    "https://www.bitmex.com/api/v1/orderBook/L2"
        |> Erl.parse
        |> Erl.setQuery "symbol" (Util.pairToString options.pair)
        |> Erl.setQuery "depth" (toString <| abs options.depth)
        |> Erl.toString


decoder : Pair -> Decoder OrderBook
decoder pair =
    (list orderDecoder)
        |> andThen (succeed << toOrderBook pair)


type Side
    = Sell
    | Buy


sideFromString : String -> Result String Side
sideFromString str =
    case String.toUpper str of
        "SELL" ->
            Ok Sell

        "BUY" ->
            Ok Buy

        _ ->
            Err <| "'" ++ str ++ "' is not a valid side"


sideDecoder : Decoder Side
sideDecoder =
    customDecoder string sideFromString


type alias BitMexOrder =
    { symbol : String
    , id : Int
    , side : Side
    , size : Float
    , price : Float
    }


orderDecoder : Decoder BitMexOrder
orderDecoder =
    decode BitMexOrder
        |> required "symbol" string
        |> required "id" int
        |> required "side" sideDecoder
        |> required "size" float
        |> required "price" float


toOrderBook : Pair -> List BitMexOrder -> OrderBook
toOrderBook pair orders =
    let
        asks =
            List.filter (\o -> o.side == Sell) orders

        bids =
            List.filter (\o -> o.side == Buy) orders

        normalizeOrder order =
            { marketName = BitMex
            , amount = order.size
            , price = order.price
            }
    in
        { pair = pair
        , asks = List.map normalizeOrder asks
        , bids = List.map normalizeOrder bids
        }
