module Markets
    exposing
        ( all
        , poloniex
        , State
        , init
        , update
        )

{-|

# Markets
@docs all, poloniex

# Config
@docs Config, config

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
            case request of
                PairsRequest market _ ->
                    market

                OrderBooksRequest market _ ->
                    market

                RecentTradesRequest market _ ->
                    market

        oldMarketState =
            state
                |> EveryDict.get market.name
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
            |> EveryDict.insert market.name newMarketState
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
                |> List.map (\marketState -> ( marketState.market.name, marketState ))
                |> EveryDict.fromList

        requests =
            List.filterMap identity maybeRequests
    in
        ( State newState, requests )


checkMarketState : Time -> MarketState -> ( MarketState, Maybe Request )
checkMarketState now marketState =
    if now - marketState.lastFetched > marketState.market.rateLimit then
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


{-| Send a Market request
-}
send : (Response -> msg) -> Request -> Cmd msg
send toMsg request =
    case request of
        PairsRequest market task ->
            attempt toMsg market PairsResponse task

        OrderBooksRequest market task ->
            attempt toMsg market OrderBooksResponse task

        RecentTradesRequest market task ->
            attempt toMsg market RecentTradesResponse task


attempt : (Response -> msg) -> Market -> (MarketName -> x -> Response) -> Task Error x -> Cmd msg
attempt toMsg market toResponse =
    Task.attempt
        (\result ->
            case result of
                Ok x ->
                    toMsg <| toResponse (Market.marketName market) x

                Err error ->
                    toMsg <| ErrorResponse (Market.marketName market) error
        )
