module GeohashEncode exposing (encode)

import String
import Array exposing (get)
import Bitwise
import Base32Codes exposing (..)


type alias EncodeState =
    { chars : List Char
    , bits : Int
    , bitsTotal : Int
    , hashValue : Int
    , maxLat : Float
    , minLat : Float
    , maxLon : Float
    , minLon : Float
    , latitude : Float
    , longitude : Float
    }


encode : Float -> Float -> Int -> String
encode latitude longitude precision =
    let
        initialState : EncodeState
        initialState =
            { chars = []
            , bits = 0
            , bitsTotal = 0
            , hashValue = 0
            , maxLat = 90
            , minLat = -90
            , maxLon = 180
            , minLon = -180
            , latitude = latitude
            , longitude = longitude
            }
    in
        encodeRecursive precision initialState
            |> .chars
            |> String.fromList


encodeRecursive : Int -> EncodeState -> EncodeState
encodeRecursive precision state =
    if List.length state.chars < precision then
        encodeTransform state
            |> encodeAddChar
            |> encodeRecursive precision
    else
        state


encodeTransform : EncodeState -> EncodeState
encodeTransform state =
    let
        newState : EncodeState
        newState =
            if state.bitsTotal % 2 == 0 then
                modifyLongitude state
            else
                modifyLatitude state
    in
        { newState
            | bits = newState.bits + 1
            , bitsTotal = newState.bitsTotal + 1
        }


encodeAddChar : EncodeState -> EncodeState
encodeAddChar state =
    let
        char : Char
        char =
            get state.hashValue base32CodesArray
                |> Maybe.withDefault ' '
    in
        if state.bits == 5 then
            { state
                | chars = state.chars ++ [ char ]
                , bits = 0
                , hashValue = 0
            }
        else
            state


modifyLongitude : EncodeState -> EncodeState
modifyLongitude state =
    let
        mid : Float
        mid =
            (state.maxLon + state.minLon) / 2
    in
        if state.longitude > mid then
            { state
                | hashValue = (Bitwise.shiftLeftBy 1 state.hashValue) + 1
                , minLon = mid
            }
        else
            { state
                | hashValue = (Bitwise.shiftLeftBy 1 state.hashValue)
                , maxLon = mid
            }


modifyLatitude : EncodeState -> EncodeState
modifyLatitude state =
    let
        mid : Float
        mid =
            (state.maxLat + state.minLat) / 2
    in
        if state.latitude > mid then
            { state
                | hashValue = (Bitwise.shiftLeftBy 1 state.hashValue) + 1
                , minLat = mid
            }
        else
            { state
                | hashValue = (Bitwise.shiftLeftBy 1 state.hashValue)
                , maxLat = mid
            }
