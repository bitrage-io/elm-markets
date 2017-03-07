module Markets.Bitfinex exposing (..)

import Http
import Market.PublicApi as PublicApi exposing (PublicApi)
import Markets.Bitfinex.PairList as PairList
import Markets.Bitfinex.OrderBook as OrderBook
import Markets.Bitfinex.TradeHistory as TradeHistory
import Model.Pair exposing (Pair)
import Model.MarketName exposing (MarketName(Bitfinex))
import Model.OrderBook exposing (OrderBook)
import Model.Trade exposing (Trade)
import Task exposing (Task)
import Time exposing (Time)


publicApi : PublicApi
publicApi =
    { marketName = Bitfinex
    , rateLimit = 3 * Time.second
    , pairList = Http.toTask <| Http.get PairList.url PairList.decoder
    , orderBooks = List.map orderBook
    , recentTradeHistory = recentTradeHistory
    , fullTradeHistory = fullTradeHistory
    }


orderBook : Pair -> Task PublicApi.Error (List OrderBook)
orderBook pair =
    { pair = pair
    , depth = PublicApi.defaultOrderBookSize
    , group = False
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
    , time = Nothing
    , limit = PublicApi.defaultTradeHistorySize
    }
        |> TradeHistory.url
        |> (flip Http.get) (TradeHistory.decoder pair)
        |> Http.toTask


fullTradeHistory : Time -> List Pair -> List (Task PublicApi.Error (List Trade))
fullTradeHistory now pairs =
    []
