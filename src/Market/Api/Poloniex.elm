module Market.Api.Poloniex exposing (..)

import Http
import Market
import Market.Api
import Market.Api.Poloniex.Pairs as Pairs
import Market.Api.Poloniex.OrderBooks as OrderBooks
import Market.Api.Poloniex.Trades as Trades


api : Market.Api.Api
api =
    Market.Api.http
        { market = Market.Poloniex
        , pairs = pairs
        , orderBooks = orderBooks
        , trades = trades
        }


pairs : Http.Request (List Market.Pair)
pairs =
    Http.get Pairs.url Pairs.decoder


orderBooks : Market.Api.OrderBooksOptions -> List (Http.Request (List Market.OrderBook))
orderBooks { depth } =
    { pair = Nothing
    , depth = depth
    }
        |> OrderBooks.url
        |> (flip Http.get) (OrderBooks.decoder Nothing)
        |> (flip (::)) []


trades : Market.Api.TradesOptions -> List (Http.Request (List Market.Trade))
trades { pairs } =
    List.map tradesSingle pairs


tradesSingle : Market.Pair -> Http.Request (List Market.Trade)
tradesSingle pair =
    { pair = pair
    , start = Nothing
    , end = Nothing
    }
        |> Trades.url
        |> (flip Http.get) (Trades.decoder pair)
