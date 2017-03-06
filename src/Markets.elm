module Markets
    exposing
        ( all
        , poloniex
        , State
        )

{-|

# Markets
@docs all, poloniex


# State
@docs State

-}

import EveryDict exposing (EveryDict)
import Market exposing (..)
import Markets.Poloniex as Poloniex
import Task exposing (Task)
import Time exposing (Time)


{-| All markets
-}
all : List Market
all =
    [ poloniex
    ]


{-| Poloniex
-}
poloniex : Market
poloniex =
    Poloniex.market


{-| Markets state
-}
type State
    = State (EveryDict MarketName MarketState)


initialState : List Market -> State
initialState markets =
    markets
        |> List.map (\m -> ( Market.marketName m, initialMarketState m ))
        |> EveryDict.fromList
        |> State


{-| Represents a queue for market API commands.
-}
type alias MarketState =
    { market : Market
    , requests : List Request
    , lastFetched : Time
    }


initialMarketState : Market -> MarketState
initialMarketState market =
    MarketState market [] 0


{-| Adds a request to the queue
-}
queue : State -> Request -> State
queue (State state) request =
    let
        market =
            requestMarket request

        marketName =
            Market.marketName market

        oldMarketState =
            state
                |> EveryDict.get marketName
                |> Maybe.withDefault (initialMarketState market)

        newMarketState =
            { oldMarketState
                | requests =
                    List.concat
                        [ oldMarketState.requests
                        , [ request ]
                        ]
            }
    in
        state
            |> EveryDict.insert marketName newMarketState
            |> State


{-| Checks market states for new requests
-}
check : Time -> State -> ( State, List Request )
check now (State state) =
    let
        ( marketStates, maybeRequests ) =
            state
                |> EveryDict.toList
                |> List.map (checkMarketState now << Tuple.second)
                |> List.unzip

        newState =
            marketStates
                |> List.map (\marketState -> ( Market.marketName marketState.market, marketState ))
                |> EveryDict.fromList

        requests =
            List.filterMap identity maybeRequests
    in
        ( State newState, requests )


checkMarketState : Time -> MarketState -> ( MarketState, Maybe Request )
checkMarketState now marketState =
    if now - marketState.lastFetched > Market.rateLimit marketState.market then
        let
            request =
                List.head marketState.requests

            requests =
                marketState.requests
                    |> List.tail
                    |> Maybe.withDefault []
        in
            ( { marketState
                | lastFetched = now
                , requests = requests
              }
            , request
            )
    else
        ( marketState, Nothing )


send : (Result Error Response -> msg) -> Request -> Cmd msg
send toMsg request =
    Task.attempt toMsg <| task request


requestMarket : Request -> Market
requestMarket request =
    case request of
        PairsRequest market _ ->
            market

        OrderBooksRequest market _ ->
            market

        RecentTradesRequest market _ ->
            market


task : Request -> Task Error Response
task request =
    case request of
        PairsRequest market task ->
            Task.map (PairsResponse market) task

        OrderBooksRequest market task ->
            Task.map (OrderBooksResponse market) task

        RecentTradesRequest market task ->
            Task.map (RecentTradesResponse market) task
