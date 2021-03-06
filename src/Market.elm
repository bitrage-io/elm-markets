module Market
    exposing
        ( Market(..)
        , emptyMarket
        , marketToString
        , marketFromString
        , Pair
        , emptyPair
        , pairToString
        , pairFromString
        , Symbol(..)
        , emptySymbol
        , symbolToString
        , symbolFromString
        , Side
        , sideToString
        , sideFromString
        , Order
        , emptyOrder
        , OrderBook
        , Trade
        )

{-|

# Market
@docs Market, emptyMarket, marketToString, marketFromString

# Market.Api
@docs Market.Api, api, Error, Request, Response, rateLimit

# Symbols
@docs Symbol, emptySymbol, symbolToString, symbolFromString

# Pairs
@docs Pair, emptyPair, pairToString, pairFromString

# Side
@docs Side, sideToString, sideFromString

# Orders
@docs Order, emptyOrder, OrderBook

# Trades
@docs Trade

-}

import Date exposing (Date)


{-| Represents the name of an exchange.
-}
type Market
    = UnknownMarket String
    | Bitfinex
    | BitMex
    | Bitstamp
    | Btce
    | Ccex
    | Gdax
    | HitBtc
    | Kraken
    | Poloniex


{-| Represents an order to buy or sell at a target price.
-}
type alias Order =
    { priceStr : String
    , price : Float
    , volumeStr : String
    , volume : Float
    }


{-| -}
emptyOrder : Order
emptyOrder =
    Order "" -1 "" -1


{-| Represents all the unfullfilled orders currently on a api.
-}
type alias OrderBook =
    { market : Market
    , pair : Pair
    , lowestAsk : Order
    , otherAsks : List Order
    , highestBid : Order
    , otherBids : List Order
    }


{-| Represents an executed trade.
-}
type alias Trade =
    { id : Maybe String
    , market : Market
    , pair : Pair
    , date : Date
    , side : Side
    , priceStr : String
    , price : Float
    , volumeStr : String
    , volume : Float
    }


{-| A placeholder Market
-}
emptyMarket : Market
emptyMarket =
    UnknownMarket "Empty"


{-| Turns a Market into a display-friendly String.
-}
marketToString : Market -> String
marketToString market =
    case market of
        UnknownMarket str ->
            str

        Btce ->
            "BTC-E"

        Ccex ->
            "C-CEX"

        Gdax ->
            "GDAX"

        HitBtc ->
            "HitBTC"

        _ ->
            toString market


{-| Attempts to turn a string into a Market.
-}
marketFromString : String -> Result String Market
marketFromString str =
    case String.toLower str of
        "bitfinex" ->
            Ok Bitfinex

        "bitmex" ->
            Ok BitMex

        "bitstamp" ->
            Ok Bitstamp

        "btce" ->
            Ok Btce

        "ccex" ->
            Ok Ccex

        "gdax" ->
            Ok Gdax

        "hitbtc" ->
            Ok HitBtc

        "kraken" ->
            Ok Kraken

        "poloniex" ->
            Ok Poloniex

        _ ->
            Ok <| UnknownMarket str


{-| -}
type Symbol
    = UnknownSymbol String
    | N_1CR
    | ABY
    | ADN
    | AMP
    | ARDR
    | BBR
    | BCN
    | BCY
    | BELA
    | BFX
    | BITCNY
    | BITS
    | BITUSD
    | BLK
    | BLOCK
    | BTC
    | BTCD
    | BTM
    | BTS
    | BURST
    | C2
    | CAD
    | CGA
    | CLAM
    | CNMT
    | CNY
    | CURE
    | DAO
    | DASH
    | DCR
    | DGB
    | DGD
    | DIEM
    | DOGE
    | DSH
    | EMC
    | EMC2
    | ETC
    | ETH
    | EUR
    | EXP
    | FCT
    | FIBRE
    | FLDC
    | FLO
    | FLT
    | GAME
    | GAP
    | GBP
    | GEO
    | GRC
    | GRS
    | HUC
    | HYP
    | HZ
    | ICN
    | IOC
    | JPY
    | LBC
    | LQD
    | LSK
    | LTBC
    | LTC
    | MAID
    | MCN
    | MINT
    | MMC
    | MMNXT
    | MYR
    | NAUT
    | NAV
    | NBT
    | NEOS
    | NMC
    | NOBL
    | NOTE
    | NSR
    | NXC
    | NXT
    | OMNI
    | PIGGY
    | PINK
    | POT
    | PPC
    | PTS
    | QBK
    | QORA
    | QTL
    | RADS
    | RBY
    | RDD
    | REP
    | RIC
    | RUB
    | RRT
    | SBD
    | SC
    | SDC
    | SILK
    | SJCX
    | STEEM
    | STR
    | STRAT
    | SWARM
    | SYNC
    | SYS
    | UNITY
    | USD
    | VIA
    | VOX
    | VRC
    | VTC
    | WDC
    | XBC
    | XC
    | XCN
    | XCP
    | XCR
    | XDN
    | XEM
    | XLM
    | XMG
    | XMR
    | XPB
    | XPM
    | XRP
    | XST
    | XUSD
    | XVC
    | YACC
    | ZEC


{-| A placeholder symbol
-}
emptySymbol : Symbol
emptySymbol =
    UnknownSymbol "Empty"


{-| Converts a Symbol to a display-friendly String.
-}
symbolToString : Symbol -> String
symbolToString symbol =
    case symbol of
        UnknownSymbol name ->
            name

        N_1CR ->
            "1CR"

        _ ->
            toString symbol


{-| Attempts to parse a Symbol from a String
-}
symbolFromString : String -> Result String Symbol
symbolFromString str =
    case String.toUpper str of
        "1CR" ->
            Ok N_1CR

        "N_1CR" ->
            Ok N_1CR

        "ABY" ->
            Ok ABY

        "ADN" ->
            Ok ADN

        "AMP" ->
            Ok AMP

        "ARDR" ->
            Ok ARDR

        "BBR" ->
            Ok BBR

        "BCN" ->
            Ok BCN

        "BCY" ->
            Ok BCY

        "BELA" ->
            Ok BELA

        "BFX" ->
            Ok BFX

        "BITCNY" ->
            Ok BITCNY

        "BITS" ->
            Ok BITS

        "BITUSD" ->
            Ok BITUSD

        "BLK" ->
            Ok BLK

        "BLOCK" ->
            Ok BLOCK

        "BTC" ->
            Ok BTC

        "BTCD" ->
            Ok BTCD

        "BTM" ->
            Ok BTM

        "BTS" ->
            Ok BTS

        "BURST" ->
            Ok BURST

        "C2" ->
            Ok C2

        "CAD" ->
            Ok CAD

        "CGA" ->
            Ok CGA

        "CLAM" ->
            Ok CLAM

        "CNMT" ->
            Ok CNMT

        "CNY" ->
            Ok CNY

        "CURE" ->
            Ok CURE

        "DAO" ->
            Ok DAO

        "DASH" ->
            Ok DASH

        "DCR" ->
            Ok DCR

        "DGB" ->
            Ok DGB

        "DGD" ->
            Ok DGD

        "DIEM" ->
            Ok DIEM

        "DOGE" ->
            Ok DOGE

        "XDG" ->
            Ok DOGE

        "DSH" ->
            Ok DSH

        "EMC" ->
            Ok EMC

        "EMC2" ->
            Ok EMC2

        "ETC" ->
            Ok ETC

        "ETH" ->
            Ok ETH

        "EUR" ->
            Ok EUR

        "EXP" ->
            Ok EXP

        "FCT" ->
            Ok FCT

        "FIBRE" ->
            Ok FIBRE

        "FLDC" ->
            Ok FLDC

        "FLO" ->
            Ok FLO

        "FLT" ->
            Ok FLT

        "GAME" ->
            Ok GAME

        "GAP" ->
            Ok GAP

        "GBP" ->
            Ok GBP

        "GEO" ->
            Ok GEO

        "GRC" ->
            Ok GRC

        "GRS" ->
            Ok GRS

        "HUC" ->
            Ok HUC

        "HYP" ->
            Ok HYP

        "HZ" ->
            Ok HZ

        "ICN" ->
            Ok ICN

        "IOC" ->
            Ok IOC

        "JPY" ->
            Ok JPY

        "LBC" ->
            Ok LBC

        "LQD" ->
            Ok LQD

        "LSK" ->
            Ok LSK

        "LTBC" ->
            Ok LTBC

        "LTC" ->
            Ok LTC

        "MAID" ->
            Ok MAID

        "MCN" ->
            Ok MCN

        "MINT" ->
            Ok MINT

        "MMC" ->
            Ok MMC

        "MMNXT" ->
            Ok MMNXT

        "MYR" ->
            Ok MYR

        "NAUT" ->
            Ok NAUT

        "NAV" ->
            Ok NAV

        "NBT" ->
            Ok NBT

        "NEOS" ->
            Ok NEOS

        "NMC" ->
            Ok NMC

        "NOBL" ->
            Ok NOBL

        "NOTE" ->
            Ok NOTE

        "NSR" ->
            Ok NSR

        "NXT" ->
            Ok NXT

        "NXC" ->
            Ok NXC

        "OMNI" ->
            Ok OMNI

        "PIGGY" ->
            Ok PIGGY

        "PINK" ->
            Ok PINK

        "POT" ->
            Ok POT

        "PPC" ->
            Ok PPC

        "PTS" ->
            Ok PTS

        "QBK" ->
            Ok QBK

        "QORA" ->
            Ok QORA

        "QTL" ->
            Ok QTL

        "RADS" ->
            Ok RADS

        "RBY" ->
            Ok RBY

        "RDD" ->
            Ok RDD

        "REP" ->
            Ok REP

        "RIC" ->
            Ok RIC

        "RRT" ->
            Ok RRT

        "RUB" ->
            Ok RUB

        "SBD" ->
            Ok SBD

        "SC" ->
            Ok SC

        "SDC" ->
            Ok SDC

        "SILK" ->
            Ok SILK

        "SJCX" ->
            Ok SJCX

        "STRAT" ->
            Ok STRAT

        "STEEM" ->
            Ok STEEM

        "STR" ->
            Ok STR

        "SWARM" ->
            Ok SWARM

        "SYNC" ->
            Ok SYNC

        "SYS" ->
            Ok SYS

        "UNITY" ->
            Ok UNITY

        "USD" ->
            Ok USD

        "USDT" ->
            Ok USD

        "VIA" ->
            Ok VIA

        "VOX" ->
            Ok VOX

        "VRC" ->
            Ok VRC

        "VTC" ->
            Ok VTC

        "WDC" ->
            Ok WDC

        "XBC" ->
            Ok XBC

        "XBT" ->
            Ok BTC

        "XBTC" ->
            Ok BTC

        "XC" ->
            Ok XC

        "XCN" ->
            Ok XCN

        "XCP" ->
            Ok XCP

        "XCR" ->
            Ok XCR

        "XDN" ->
            Ok XDN

        "XEM" ->
            Ok XEM

        "XLM" ->
            Ok XLM

        "XMG" ->
            Ok XMG

        "XMR" ->
            Ok XMR

        "XPB" ->
            Ok XPB

        "XPM" ->
            Ok XPM

        "XRP" ->
            Ok XRP

        "XST" ->
            Ok XST

        "XUSD" ->
            Ok XUSD

        "XVC" ->
            Ok XVC

        "YACC" ->
            Ok YACC

        "ZEC" ->
            Ok ZEC

        _ ->
            Ok <| UnknownSymbol str


{-| Represents a tradeable pair such as BSD/USD, XAU/USD, etc.
-}
type alias Pair =
    { base : Symbol
    , quote : Symbol
    }


{-| A placeholder pair
-}
emptyPair : Pair
emptyPair =
    { base = emptySymbol
    , quote = emptySymbol
    }


{-| Turns a Pair into a display-friendly String.
-}
pairToString : Pair -> String
pairToString pair =
    (symbolToString pair.base) ++ "/" ++ (symbolToString pair.quote)


{-| Attemps to turn a String into a Pair
-}
pairFromString : String -> Result String Pair
pairFromString str =
    case String.split "/" str of
        [ baseStr, quoteStr ] ->
            Result.map2 Pair
                (symbolFromString baseStr)
                (symbolFromString quoteStr)

        _ ->
            Err <| "'" ++ str ++ "' could not be converted to a pair"


{-| Represents a long/short trade side.
-}
type Side
    = Buy
    | Sell


{-| Attempts to parse a Side from a String.
-}
sideFromString : String -> Result String Side
sideFromString str =
    case String.toLower str of
        "buy" ->
            Ok Buy

        "bid" ->
            Ok Buy

        "b" ->
            Ok Buy

        "ask" ->
            Ok Sell

        "sell" ->
            Ok Sell

        "s" ->
            Ok Sell

        _ ->
            Err ("'" ++ str ++ "' is not a valid trade position")


{-| Converts a Side to a String
-}
sideToString : Side -> String
sideToString =
    toString
