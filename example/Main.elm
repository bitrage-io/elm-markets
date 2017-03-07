module Main exposing (..)

import Dict
import Html
import Market
import Market.Api
import Markets
import Time


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { pairs : List Market.Pair
    , orderBooks : Dict.Dict String Market.OrderBook
    , error : Maybe Market.Api.Error
    }


emptyModel : Model
emptyModel =
    { pairs = []
    , orderBooks = Dict.empty
    , error = Nothing
    }


type Msg
    = Fetch Time.Time
    | PairsResponse (Result Market.Api.Error (List Market.Pair))
    | OrderBooksResponse (Result Market.Api.Error (List Market.OrderBook))


init : ( Model, Cmd Msg )
init =
    emptyModel
        ! [ Market.Api.pairs PairsResponse Markets.poloniex
          , Market.Api.orderBooks OrderBooksResponse Markets.poloniex emptyModel.pairs
          ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Fetch _ ->
            model
                ! [ Market.Api.orderBooks OrderBooksResponse Markets.poloniex model.pairs
                  ]

        PairsResponse (Ok pairs) ->
            { model | pairs = pairs }
                ! [ Market.Api.orderBooks OrderBooksResponse Markets.poloniex pairs ]

        PairsResponse (Err error) ->
            { model | error = Just error } ! []

        OrderBooksResponse (Ok orderBooks) ->
            { model
                | orderBooks =
                    orderBooks
                        |> List.map (\o -> ( Market.pairToString o.pair, o ))
                        |> Dict.fromList
                        |> (flip Dict.union) model.orderBooks
            }
                ! []

        OrderBooksResponse (Err error) ->
            { model | error = Just error } ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (5 * Time.second) Fetch


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.h1 [] [ Html.text "elm-markets example" ]
        , Html.h2 [] [ Html.text "Pairs" ]
        , Html.table []
            [ Html.thead []
                [ Html.tr []
                    [ Html.th [] [ Html.text "Pair" ]
                    , Html.th [] [ Html.text "Lowest Ask" ]
                    , Html.th [] [ Html.text "Highest Bid" ]
                    , Html.th [] [ Html.text "Arbitrage" ]
                    ]
                ]
            , Html.tbody [] <|
                case model.error of
                    Nothing ->
                        List.map (pairView model) model.pairs

                    Just error ->
                        [ errorView error ]
            ]
        ]


pairView : Model -> Market.Pair -> Html.Html Msg
pairView model pair =
    let
        pairStr =
            Market.pairToString pair

        maybeOrderBook =
            Dict.get pairStr model.orderBooks

        maybeLowestAsk =
            case maybeOrderBook of
                Just orderBook ->
                    orderBook.asks
                        |> List.filterMap (.price >> String.toFloat >> Result.toMaybe)
                        |> List.sort
                        |> List.head

                Nothing ->
                    Nothing

        maybeHighestBid =
            case maybeOrderBook of
                Just orderBook ->
                    orderBook.bids
                        |> List.filterMap (.price >> String.toFloat >> Result.toMaybe)
                        |> List.sort
                        |> List.reverse
                        |> List.head

                Nothing ->
                    Nothing

        maybeArbitrage =
            case ( maybeLowestAsk, maybeHighestBid ) of
                ( Just lowestAsk, Just highestBid ) ->
                    Just <| highestBid - lowestAsk

                _ ->
                    Nothing
    in
        Html.tr []
            [ Html.th [] [ Html.text pairStr ]
            , Html.td []
                [ maybeLowestAsk
                    |> Maybe.andThen (Just << toString)
                    |> Maybe.withDefault "?"
                    |> Html.text
                ]
            , Html.td []
                [ maybeHighestBid
                    |> Maybe.andThen (Just << toString)
                    |> Maybe.withDefault "?"
                    |> Html.text
                ]
            , Html.td []
                [ maybeArbitrage
                    |> Maybe.andThen (Just << toString)
                    |> Maybe.withDefault "?"
                    |> Html.text
                , Html.text <| Market.symbolToString pair.quote
                ]
            ]


errorView : Market.Api.Error -> Html.Html Msg
errorView error =
    Html.strong []
        [ Html.text <| toString error ]
