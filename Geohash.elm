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
