----------------------------------------------------------------------
--
-- Geohash.elm
-- Geohash for Elm
-- Copyright (c) 2017 andys8
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module Geohash exposing (encode)

{-| This module is an Geohash Elm implementation.

Thanks to [Ning Sun](https://github.com/sunng87) for the [JavaScript implementation](https://github.com/sunng87/node-geohash).

# What is a geohash?
The geohash preserves **spatial locality** so that points close to each other in space are close to each other on disk. This is because the arrangement of the result is comparable with space filling **Z-order curves**. The length of geohashes can be chosen individually and depending on the degree of accuracy. Characters at the end are less significant. Truncating the geohash can be used to cover larger areas. In fact this can be used to build range queries based on the prefix of the primary key.

The geohash is constructed bitwise. The range of both dimensions will be cut in half. If the target point is located in the greater half of the range, the value of the first bit is `1`. Otherwise it’s `0`. The example longitude `11.53..°` would result in a `1-bit` as first value because it’s part of range `[0°, +180°]` and not `[-180°, 0°)`. This binary partitioning approach will be repeated alternately for both axes (beginning with longitude). Because the encoding is weaving the bits together, the geohash has the spatial locality property.

# Functions
@docs encode
-}

import String
import Dict exposing (Dict)
import Array exposing (Array)
import Bitwise


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


type alias EncodeState =
    { chars : List Char
    , bits : Int
    , bitsTotal : Int
    , hash_value : Int
    , maxLat : Float
    , minLat : Float
    , maxLon : Float
    , minLon : Float
    , mid : Float
    , latitude : Float
    , longitude : Float
    }


{-| Encodes latitude, longitude, precision to geohash.

    encode 57.648 10.41 6 == "u4pruy"
-}
encode : Float -> Float -> Int -> String
encode latitude longitude numberOfChars =
    let
        encodeState : EncodeState
        encodeState =
            { chars = []
            , bits = 0
            , bitsTotal = 0
            , hash_value = 0
            , maxLat = 90
            , minLat = -90
            , maxLon = 180
            , minLon = -180
            , mid = 0
            , latitude = latitude
            , longitude = longitude
            }
    in
        (encode_ numberOfChars encodeState) |> .chars |> String.fromList


encode_ : Int -> EncodeState -> EncodeState
encode_ numberOfChars encodeState =
    if List.length encodeState.chars < numberOfChars then
        encode__ encodeState |> encodeAddChar |> (encode_ numberOfChars)
    else
        encodeState


encode__ : EncodeState -> EncodeState
encode__ encodeState =
    let
        modState =
            if encodeState.bitsTotal % 2 == 0 then
                modifyLon encodeState
            else
                modifyLat encodeState
    in
        { modState
            | bits = modState.bits + 1
            , bitsTotal = modState.bitsTotal + 1
        }


encodeAddChar : EncodeState -> EncodeState
encodeAddChar encodeState =
    let
        char =
            Array.get encodeState.hash_value base32CodesArray
    in
        if encodeState.bits == 5 then
            { encodeState
                | chars = encodeState.chars ++ [ (Maybe.withDefault ' ' char) ]
                , bits = 0
                , hash_value = 0
            }
        else
            encodeState


modifyLon : EncodeState -> EncodeState
modifyLon state =
    let
        mid =
            (state.maxLon + state.minLon) / 2
    in
        if state.longitude > mid then
            { state
                | hash_value = (Bitwise.shiftLeftBy 1 state.hash_value) + 1
                , minLon = mid
            }
        else
            { state
                | hash_value = (Bitwise.shiftLeftBy 1 state.hash_value)
                , maxLon = mid
            }


modifyLat : EncodeState -> EncodeState
modifyLat state =
    let
        mid =
            (state.maxLat + state.minLat) / 2
    in
        if state.latitude > mid then
            { state
                | hash_value = (Bitwise.shiftLeftBy 1 state.hash_value) + 1
                , minLat = mid
            }
        else
            { state
                | hash_value = (Bitwise.shiftLeftBy 1 state.hash_value)
                , maxLat = mid
            }
