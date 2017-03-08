module Market.Api exposing (..)

import Date
import Http
import Market exposing (Market)
import Task


type Api
    = Http
        { market : Market.Market
        , pairs : Request (List Market.Pair)
        , orderBooks : OrderBooksOptions -> List (Request (List Market.OrderBook))
        , trades : TradesOptions -> List (Request (List Market.Trade))
        }


http :
    { market : Market.Market
    , pairs : Http.Request (List Market.Pair)
    , orderBooks : OrderBooksOptions -> List (Http.Request (List Market.OrderBook))
    , trades : TradesOptions -> List (Http.Request (List Market.Trade))
    }
    -> Api
http api =
    Http
        { market = api.market
        , pairs = HttpRequest api.market api.pairs
        , orderBooks = \options -> List.map (HttpRequest api.market) <| api.orderBooks options
        , trades = \options -> List.map (HttpRequest api.market) <| api.trades options
        }


type Request responseType
    = HttpRequest Market.Market (Http.Request responseType)


type Response responseType
    = HttpResponse Market.Market responseType


type alias OrderBooksOptions =
    { pairs : List Market.Pair
    , depth : Int
    }


type alias TradesOptions =
    { pairs : List Market.Pair
    , before : Maybe Date.Date
    }


type Error
    = HttpError Market.Market Http.Error


{-| -}
pairsRequest : Api -> Request (List Market.Pair)
pairsRequest (Http api) =
    api.pairs


{-| -}
pairs : (Result Error (List Market.Pair) -> msg) -> Api -> Cmd msg
pairs toMsg api =
    api
        |> pairsRequest
        |> attempt toMsg


{-| -}
orderBooksRequests : OrderBooksOptions -> Api -> List (Request (List Market.OrderBook))
orderBooksRequests options (Http api) =
    api.orderBooks options


{-| -}
orderBooks :
    (Result Error (List Market.OrderBook) -> msg)
    -> OrderBooksOptions
    -> Api
    -> Cmd msg
orderBooks toMsg options api =
    api
        |> orderBooksRequests options
        |> List.map (attempt toMsg)
        |> Cmd.batch


{-| -}
tradesRequests : TradesOptions -> Api -> List (Request (List Market.Trade))
tradesRequests options (Http api) =
    api.trades options


{-| -}
trades : (Result Error (List Market.Trade) -> msg) -> TradesOptions -> Api -> Cmd msg
trades toMsg options api =
    api
        |> tradesRequests options
        |> List.map (attempt toMsg)
        |> Cmd.batch


{-| -}
send : Request responseType -> Task.Task Error responseType
send (HttpRequest market httpRequest) =
    httpRequest
        |> Http.toTask
        |> Task.mapError (HttpError market)


{-| -}
attempt : (Result Error responseType -> msg) -> Request responseType -> Cmd msg
attempt toMsg request =
    request
        |> send
        |> Task.attempt toMsg
