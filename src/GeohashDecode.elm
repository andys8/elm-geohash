module GeohashDecode exposing (decode, decodeBoundingBox)

import Base32Codes exposing (..)
import Bitwise
import Char
import Dict


type alias DecodeState =
    { isLon : Bool
    , maxLat : Float
    , minLat : Float
    , maxLon : Float
    , minLon : Float
    }


decode : String -> { lat : Float, lon : Float, latError : Float, lonError : Float }
decode hashValue =
    let
        boundingBox =
            decodeBoundingBox hashValue

        lat =
            (boundingBox.minLat + boundingBox.maxLat) / 2

        lon =
            (boundingBox.minLon + boundingBox.maxLon) / 2
    in
    { lat = lat
    , lon = lon
    , latError = boundingBox.maxLat - lat
    , lonError = boundingBox.maxLon - lon
    }


decodeBoundingBox : String -> { minLat : Float, minLon : Float, maxLat : Float, maxLon : Float }
decodeBoundingBox hashString =
    let
        hashValues =
            convertHashToBase32Code hashString

        initialState : DecodeState
        initialState =
            { isLon = True
            , maxLat = 90
            , minLat = -90
            , maxLon = 180
            , minLon = -180
            }

        resultState =
            decodeIterateHashValues hashValues initialState
    in
    { minLat = resultState.minLat
    , minLon = resultState.minLon
    , maxLat = resultState.maxLat
    , maxLon = resultState.maxLon
    }


convertHashToBase32Code : String -> List Int
convertHashToBase32Code hashString =
    String.toList hashString
        |> List.filterMap convertCharToBase32Code


convertCharToBase32Code : Char -> Maybe Int
convertCharToBase32Code char =
    Dict.get (Char.toLower char) base32CodesDict


decodeIterateHashValues : List Int -> DecodeState -> DecodeState
decodeIterateHashValues hashValues state =
    case hashValues of
        [] ->
            state

        hashValue :: remainingHashValues ->
            decodeIterateBits hashValue state
                |> decodeIterateHashValues remainingHashValues


decodeIterateBits : Int -> DecodeState -> DecodeState
decodeIterateBits hashValue =
    decodeIterate hashValue 4


decodeIterate : Int -> Int -> DecodeState -> DecodeState
decodeIterate hashValue bits state =
    if bits < 0 then
        state

    else
        processState hashValue bits state
            |> switchIsLon
            |> decodeIterate hashValue (bits - 1)


switchIsLon : DecodeState -> DecodeState
switchIsLon state =
    { state | isLon = not state.isLon }


processState : Int -> Int -> DecodeState -> DecodeState
processState hashValue bits state =
    let
        bit : Int
        bit =
            Bitwise.and (Bitwise.shiftRightBy bits hashValue) 1
    in
    if state.isLon then
        let
            mid =
                (state.maxLon + state.minLon) / 2
        in
        if bit == 1 then
            { state | minLon = mid }

        else
            { state | maxLon = mid }

    else
        let
            mid =
                (state.maxLat + state.minLat) / 2
        in
        if bit == 1 then
            { state | minLat = mid }

        else
            { state | maxLat = mid }
