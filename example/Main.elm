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
    { apis : List Market.Api.Api
    , orderBooks : Dict.Dict String (List Market.OrderBook)
    , error : Maybe Market.Api.Error
    }


emptyModel : Model
emptyModel =
    { apis = Markets.browserSafe
    , orderBooks = Dict.empty
    , error = Nothing
    }


type Msg
    = Fetch Time.Time
    | PairsResponse (Result Market.Api.Error (List Market.Pair))
    | OrderBooksResponse (Result Market.Api.Error (List Market.OrderBook))


init : ( Model, Cmd Msg )
init =
    emptyModel ! List.map (Market.Api.pairs PairsResponse) emptyModel.apis


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Fetch _ ->
            model
                ! List.map (Market.Api.orderBooks OrderBooksResponse { pairs = [], depth = 100 }) model.apis

        PairsResponse (Ok pairs) ->
            let
                orderBooks =
                    pairs
                        |> List.map (\p -> ( Market.pairToString p, [] ))
                        |> Dict.fromList
                        |> Dict.union model.orderBooks
            in
                { model | orderBooks = orderBooks }
                    ! List.map (Market.Api.orderBooks OrderBooksResponse { pairs = pairs, depth = 100 }) model.apis

        PairsResponse (Err error) ->
            { model | error = Just error } ! []

        OrderBooksResponse (Ok orderBooks) ->
            { model | orderBooks = List.foldl updateOrderBooks model.orderBooks orderBooks } ! []

        OrderBooksResponse (Err error) ->
            { model | error = Just error } ! []


updateOrderBooks :
    Market.OrderBook
    -> Dict.Dict String (List Market.OrderBook)
    -> Dict.Dict String (List Market.OrderBook)
updateOrderBooks orderBook allOrderBooks =
    let
        pairStr =
            Market.pairToString orderBook.pair

        pairOrderBooks =
            allOrderBooks
                |> Dict.get pairStr
                |> Maybe.withDefault []
                |> List.filter (\ob -> ob.market == orderBook.market)
                |> (::) orderBook
    in
        Dict.insert pairStr pairOrderBooks allOrderBooks


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
                        List.map orderBooksView <| Dict.toList model.orderBooks

                    Just error ->
                        [ errorView error ]
            ]
        ]


orderBooksView : ( String, List Market.OrderBook ) -> Html.Html Msg
orderBooksView ( pairStr, orderBooks ) =
    let
        maybeLowestAsk =
            orderBooks
                |> List.sortBy (\ob -> ob.lowestAsk.price)
                |> List.head
                |> Maybe.andThen (Just << .lowestAsk)

        maybeHighestBid =
            orderBooks
                |> List.sortBy (\ob -> ob.highestBid.price)
                |> List.reverse
                |> List.head
                |> Maybe.andThen (Just << .highestBid)

        maybeArbitrage =
            case ( maybeLowestAsk, maybeHighestBid ) of
                ( Just lowestAsk, Just highestBid ) ->
                    Just <| highestBid.price - lowestAsk.price

                _ ->
                    Nothing
    in
        Html.tr []
            [ Html.th [] [ Html.text pairStr ]
            , Html.td []
                [ maybeLowestAsk
                    |> Maybe.andThen (Just << .priceStr)
                    |> Maybe.withDefault "?"
                    |> Html.text
                ]
            , Html.td []
                [ maybeHighestBid
                    |> Maybe.andThen (Just << .priceStr)
                    |> Maybe.withDefault "?"
                    |> Html.text
                ]
            , Html.td []
                [ maybeArbitrage
                    |> Maybe.andThen (Just << toString)
                    |> Maybe.withDefault "?"
                    |> Html.text
                ]
            ]


errorView : Market.Api.Error -> Html.Html Msg
errorView error =
    Html.strong []
        [ Html.text <| toString error ]
