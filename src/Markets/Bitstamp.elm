module Markets.Bitstamp exposing (..)

import Http
import Market.PublicApi as PublicApi exposing (PublicApi)
import Markets.Bitstamp.PairList as PairList
import Markets.Bitstamp.OrderBook as OrderBook
import Markets.Bitstamp.TradeHistory as TradeHistory
import Model.Pair exposing (Pair)
import Model.MarketName exposing (MarketName(Bitstamp))
import Model.OrderBook exposing (OrderBook)
import Model.Trade exposing (Trade)
import Task exposing (Task)
import Time exposing (Time)


publicApi : PublicApi
publicApi =
    { marketName = Bitstamp
    , rateLimit = 3 * Time.second
    , pairList = Task.succeed PairList.pairs
    , orderBooks = List.map orderBook
    , recentTradeHistory = recentTradeHistory
    , fullTradeHistory = fullTradeHistory
    }


orderBook : Pair -> Task PublicApi.Error (List OrderBook)
orderBook pair =
    { pair = pair
    }
        |> OrderBook.url
        |> (flip Http.get) (OrderBook.decoder pair)
        |> Http.toTask
        |> Task.map (\book -> [ book ])


recentTradeHistory : Time -> List Pair -> List (Task PublicApi.Error (List Trade))
recentTradeHistory _ =
    List.map recentTradeHistorySingle


recentTradeHistorySingle : Pair -> Task PublicApi.Error (List Trade)
recentTradeHistorySingle pair =
    { pair = pair
    , time = TradeHistory.Day
    }
        |> TradeHistory.url
        |> (flip Http.get) (TradeHistory.decoder pair)
        |> Http.toTask


fullTradeHistory : Time -> List Pair -> List (Task PublicApi.Error (List Trade))
fullTradeHistory now pairs =
    []
