module Markets.Btce exposing (..)

import Http
import Market.PublicApi as PublicApi exposing (PublicApi)
import Markets.Btce.PairList as PairList
import Markets.Btce.OrderBook as OrderBook
import Markets.Btce.TradeHistory as TradeHistory
import Model.Pair exposing (Pair)
import Model.MarketName exposing (MarketName(Btce))
import Model.OrderBook exposing (OrderBook)
import Model.Trade exposing (Trade)
import Task exposing (Task)
import Time exposing (Time)


publicApi : PublicApi
publicApi =
    { marketName = Btce
    , rateLimit = 3 * Time.second
    , pairList = Http.toTask <| Http.get PairList.url PairList.decoder
    , orderBooks = orderBooks
    , recentTradeHistory = recentTradeHistory
    , fullTradeHistory = fullTradeHistory
    }


orderBooks : List Pair -> List (Task PublicApi.Error (List OrderBook))
orderBooks pairs =
    { pairs = pairs
    }
        |> OrderBook.url
        |> (flip Http.get) OrderBook.decoder
        |> Http.toTask
        |> (flip (::)) []


recentTradeHistory : Time -> List Pair -> List (Task PublicApi.Error (List Trade))
recentTradeHistory _ pairs =
    { pairs = pairs
    }
        |> TradeHistory.url
        |> (flip Http.get) TradeHistory.decoder
        |> Http.toTask
        |> (flip (::)) []


fullTradeHistory : Time -> List Pair -> List (Task PublicApi.Error (List Trade))
fullTradeHistory now pairs =
    []
