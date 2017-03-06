module Main exposing (..)

import Html exposing (Html, div, text, button)
import Html.Events exposing (onClick)
import Market
import Markets


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { markets : Markets.State
    }


emptyModel : Model
emptyModel =
    { markets = Markets.emptyState
    }


type Msg
    = Tick Time
    | MarketResponse Market.Response


init : ( Model, Cmd Msg )
init =
    let
        ( markets, cmd ) =
            Markets.init MarketResponse Markets.all
    in
        { emptyModel | markets = markets } ! [ cmd ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick now ->
            let
                ( markets, cmd ) =
                    Markets.update MarketResponse model.markets now
            in
                { model | markets = markets } ! [ cmd ]

        MarketResponse (Ok response) ->
            case response of
                Market.PairsResponse marketName pairs ->
                    let
                        _ =
                            Debug.log "Pairs" ( marketName, pairs )
                    in
                        model
                            ! [ Markets.orderBooks MarketResponse model.markets
                              , Markets.recentTrades MarketResponse model.markets
                              ]

                Market.OrderBooksResponse marketName orderBooks ->
                    let
                        _ =
                            Debug.log "Order books" ( marketName, orderBooks )
                    in
                        model ! []

                Market.RecentTradesResponse marketName trades ->
                    let
                        _ =
                            Debug.log "Recent trades" ( marketName, trades )
                    in
                        model ! []

        MarketResponse (Err error) ->
            let
                _ =
                    Debug.log "Error" error
            in
                model ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every Time.second Tick


view : Model -> Html Msg
view model =
    text "Exampleeeee"
