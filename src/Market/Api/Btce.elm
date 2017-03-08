module Market.Api.Btce exposing (..)

import Http
import Market
import Market.Api
import Market.Api.Btce.OrderBooks as OrderBooks
import Market.Api.Btce.Pairs as Pairs
import Market.Api.Btce.Trades as Trades


api : Market.Api.Api
api =
    Market.Api.http
        { market = Market.Btce
        , pairs = pairs
        , orderBooks = orderBooks
        , trades = trades
        }


pairs : Http.Request (List Market.Pair)
pairs =
    Http.get Pairs.url Pairs.decoder


orderBooks : Market.Api.OrderBooksOptions -> List (Http.Request (List Market.OrderBook))
orderBooks { pairs } =
    { pairs = pairs
    }
        |> OrderBooks.url
        |> (flip Http.get) OrderBooks.decoder
        |> (flip (::)) []


trades : Market.Api.TradesOptions -> List (Http.Request (List Market.Trade))
trades { pairs } =
    { pairs = pairs
    }
        |> Trades.url
        |> (flip Http.get) Trades.decoder
        |> (flip (::)) []
