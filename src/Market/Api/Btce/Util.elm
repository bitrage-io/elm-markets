module Market.Api.Btce.Util exposing (..)

import Market
import Set


pairToString : Market.Pair -> String
pairToString { base, quote } =
    String.toLower <| (toString base) ++ "_" ++ (toString quote)


pairFromString : String -> Result String Market.Pair
pairFromString pairStr =
    case String.split "_" pairStr of
        [ baseStr, quoteStr ] ->
            Result.map2 Market.Pair
                (Market.symbolFromString baseStr)
                (Market.symbolFromString quoteStr)

        _ ->
            Err <| "'" ++ pairStr ++ "' could not be converted to a pair"


joinPairs : List Market.Pair -> String
joinPairs pairs =
    pairs
        |> List.map pairToString
        |> Set.fromList
        |> Set.toList
        |> String.join "-"
