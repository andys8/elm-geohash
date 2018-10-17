module Base32Codes exposing (base32CodesArray, base32CodesDict)

import Array exposing (Array)
import Dict exposing (Dict)


base32Codes : String
base32Codes =
    "0123456789bcdefghjkmnpqrstuvwxyz"


flipTuple : a -> b -> ( b, a )
flipTuple a b =
    ( b, a )


base32CodesDict : Dict Char Int
base32CodesDict =
    String.toList base32Codes
        |> List.indexedMap flipTuple
        |> Dict.fromList


base32CodesArray : Array.Array Char
base32CodesArray =
    String.toList base32Codes
        |> Array.fromList
