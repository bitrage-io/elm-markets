module Market.Api.Poloniex exposing (..)

import Http
import Market
import Market.Api
import Market.Api.Poloniex.Pairs as Pairs
import Market.Api.Poloniex.OrderBooks as OrderBooks
import Market.Api.Poloniex.RecentTrades as RecentTrades


api : Market.Api.Api
api =
    Market.Api.http
        { market = Market.Poloniex
        , pairs = pairs
        , orderBooks = orderBooks
        , recentTrades = recentTrades
        }


pairs : Http.Request (List Market.Pair)
pairs =
    Http.get Pairs.url Pairs.decoder


orderBooks : List Market.Pair -> List (Http.Request (List Market.OrderBook))
orderBooks _ =
    { pair = Nothing
    , depth = 100
    }
        |> OrderBooks.url
        |> (flip Http.get) (OrderBooks.decoder Nothing)
        |> (flip (::)) []


recentTrades : List Market.Pair -> List (Http.Request (List Market.Trade))
recentTrades =
    List.map recentTradeHistorySingle


recentTradeHistorySingle : Market.Pair -> Http.Request (List Market.Trade)
recentTradeHistorySingle pair =
    { pair = pair
    , start = Nothing
    , end = Nothing
    }
        |> RecentTrades.url
        |> (flip Http.get) (RecentTrades.decoder pair)
