module Market.Api.Poloniex.Util exposing (..)

import Market exposing (Symbol(USD), Pair)


symbolToString : Symbol -> String
symbolToString symbol =
    case symbol of
        USD ->
            "USDT"

        _ ->
            Market.symbolToString symbol


pairToString : Pair -> String
pairToString { quote, base } =
    if quote == USD then
        (symbolToString quote) ++ "_" ++ (symbolToString base)
    else
        (symbolToString base) ++ "_" ++ (symbolToString quote)


pairFromString : String -> Result String Pair
pairFromString pairStr =
    case String.split "_" pairStr of
        [ baseStr, quoteStr ] ->
            Result.map2
                (\base quote ->
                    if base == USD then
                        Pair quote base
                    else
                        Pair base quote
                )
                (Market.symbolFromString baseStr)
                (Market.symbolFromString quoteStr)

        _ ->
            Err <| "'" ++ pairStr ++ "' could not be converted to a pair"
