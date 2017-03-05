module Markets.Poloniex exposing (market)

import Http
import Market exposing (..)
import Markets.Poloniex.PairList as PairList
import Markets.Poloniex.OrderBook as OrderBook
import Markets.Poloniex.TradeHistory as TradeHistory
import Task exposing (Task)
import Time exposing (Time)


market : Market
market =
    Market.market
        { name = Poloniex
        , rateLimit = 3 * Time.second
        , pairs = Http.toTask <| Http.get PairList.url PairList.decoder
        , orderBooks = orderBooks
        , recentTrades = recentTrades
        }


orderBooks : List Pair -> List (Task Error (List OrderBook))
orderBooks _ =
    { pair = Nothing
    , depth = 100000
    }
        |> OrderBook.url
        |> (flip Http.get) (OrderBook.decoder Nothing)
        |> Http.toTask
        |> (flip (::)) []


recentTrades : List Pair -> List (Task Error (List Trade))
recentTrades =
    List.map recentTradeHistorySingle


recentTradeHistorySingle : Pair -> Task Error (List Trade)
recentTradeHistorySingle pair =
    { pair = pair
    , start = Nothing
    , end = Nothing
    }
        |> TradeHistory.url
        |> (flip Http.get) (TradeHistory.decoder pair)
        |> Http.toTask
