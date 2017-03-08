module Market.Api.Gdax exposing (..)

import Market
import Market.Api
import Market.Api.Gdax.Pairs as Gdax
import Market.Api.Gdax.OrderBooks as Gdax
import Market.Api.Gdax.Trades as Gdax


api : Market.Api.Api
api =
    Market.Api.http
        { market = Market.Gdax
        , pairs = Gdax.pairs
        , orderBooks = Gdax.orderBooks
        , trades = Gdax.trades
        }
