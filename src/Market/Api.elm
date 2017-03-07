module Market.Api exposing (..)

import Http
import Market
import Task


type Request responseType
    = HttpRequest Market.Market (Http.Request responseType)


type Response responseType
    = HttpResponse Market.Market responseType


type Error
    = HttpError Market.Market Http.Error


type Api
    = Http
        { market : Market.Market
        , pairs : Request (List Market.Pair)
        , orderBooks : List Market.Pair -> List (Request (List Market.OrderBook))
        , recentTrades : List Market.Pair -> List (Request (List Market.Trade))
        }


http :
    { market : Market.Market
    , pairs : Http.Request (List Market.Pair)
    , orderBooks : List Market.Pair -> List (Http.Request (List Market.OrderBook))
    , recentTrades : List Market.Pair -> List (Http.Request (List Market.Trade))
    }
    -> Api
http api =
    Http
        { market = api.market
        , pairs = HttpRequest api.market api.pairs
        , orderBooks = \pairs -> List.map (HttpRequest api.market) <| api.orderBooks pairs
        , recentTrades = \pairs -> List.map (HttpRequest api.market) <| api.recentTrades pairs
        }


pairsRequest : Api -> Request (List Market.Pair)
pairsRequest (Http api) =
    api.pairs


pairs : (Result Error (List Market.Pair) -> msg) -> Api -> Cmd msg
pairs toMsg api =
    api
        |> pairsRequest
        |> attempt toMsg


orderBooksRequests : Api -> List Market.Pair -> List (Request (List Market.OrderBook))
orderBooksRequests (Http api) =
    api.orderBooks


orderBooks : (Result Error (List Market.OrderBook) -> msg) -> Api -> List Market.Pair -> Cmd msg
orderBooks toMsg api pairs =
    pairs
        |> orderBooksRequests api
        |> List.map (attempt toMsg)
        |> Cmd.batch


recentTradesRequests : Api -> List Market.Pair -> List (Request (List Market.Trade))
recentTradesRequests (Http api) =
    api.recentTrades


recentTrades : (Result Error (List Market.Trade) -> msg) -> Api -> List Market.Pair -> Cmd msg
recentTrades toMsg api pairs =
    pairs
        |> recentTradesRequests api
        |> List.map (attempt toMsg)
        |> Cmd.batch


send : Request responseType -> Task.Task Error responseType
send request =
    case request of
        HttpRequest market httpRequest ->
            httpRequest
                |> Http.toTask
                |> Task.mapError (HttpError market)


attempt : (Result Error responseType -> msg) -> Request responseType -> Cmd msg
attempt toMsg request =
    request
        |> send
        |> Task.attempt toMsg
