module Markets.Btce.Util exposing (..)

import Model.Pair as Pair exposing (Pair)
import Model.Symbol as Symbol exposing (Symbol(..))
import Set


pairToString : Pair -> String
pairToString { base, quote } =
    String.toLower <| (toString base) ++ "_" ++ (toString quote)


pairFromString : String -> Result String Pair
pairFromString pairStr =
    case String.split "_" pairStr of
        [ baseStr, quoteStr ] ->
            Result.map2 Pair
                (Symbol.fromString baseStr)
                (Symbol.fromString quoteStr)

        _ ->
            Err <| "'" ++ pairStr ++ "' could not be converted to a pair"


joinPairs : List Pair -> String
joinPairs pairs =
    pairs
        |> List.map pairToString
        |> Set.fromList
        |> Set.toList
        |> String.join "-"
