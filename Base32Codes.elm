module Base32Codes exposing (base32CodesDict, base32CodesArray)

import Dict exposing (Dict)
import Array exposing (Array)


base32Codes : String
base32Codes =
    "0123456789bcdefghjkmnpqrstuvwxyz"


base32CodesDict : Dict Char Int
base32CodesDict =
    String.toList base32Codes
        |> Array.fromList
        |> Array.indexedMap (\i -> (\a -> ( a, i )))
        |> Array.toList
        |> Dict.fromList


base32CodesArray : Array.Array Char
base32CodesArray =
    String.toList base32Codes
        |> Array.fromList
