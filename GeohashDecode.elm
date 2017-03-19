module GeohashDecode exposing (decode, decodeBoundingBox)

import Dict
import Char
import Base32Codes exposing (..)
import Bitwise


type alias DecodeState =
    { isLon : Bool
    , maxLat : Float
    , minLat : Float
    , maxLon : Float
    , minLon : Float
    }


decode : String -> { latitude : Float, longitude : Float, latitudeError : Float, longitudeError : Float }
decode hashValue =
    let
        bbox =
            decodeBoundingBox (hashValue)

        lat =
            (bbox.minLat + bbox.maxLat) / 2

        lon =
            (bbox.minLon + bbox.maxLon) / 2

        latErr =
            bbox.maxLat - lat

        lonErr =
            bbox.maxLon - lon
    in
        { latitude = lat
        , longitude = lon
        , latitudeError = latErr
        , longitudeError = lonErr
        }


decodeBoundingBox : String -> { minLat : Float, minLon : Float, maxLat : Float, maxLon : Float }
decodeBoundingBox hashString =
    let
        hashValues =
            convertHashToBase32Code hashString

        decodeState : DecodeState
        decodeState =
            { isLon = True
            , maxLat = 90
            , minLat = -90
            , maxLon = 180
            , minLon = -180
            }

        resultState =
            decodeIterateHashValues hashValues decodeState
    in
        { minLat = resultState.minLat
        , minLon = resultState.minLon
        , maxLat = resultState.maxLat
        , maxLon = resultState.maxLon
        }


decodeIterateHashValues : List Int -> DecodeState -> DecodeState
decodeIterateHashValues hashValues state =
    case hashValues of
        [] ->
            state

        x :: xs ->
            decodeIterateHashValues xs (decodeIterateBits x state)


decodeIterateBits : Int -> DecodeState -> DecodeState
decodeIterateBits hashValue state =
    decodeIterate hashValue 4 state


decodeIterate : Int -> Int -> DecodeState -> DecodeState
decodeIterate hashValue bits state =
    if bits < 0 then
        state
    else
        decodeIterate hashValue (bits - 1) (processState hashValue bits state |> switchIsLon)


switchIsLon : DecodeState -> DecodeState
switchIsLon state =
    { state | isLon = not state.isLon }


processState : Int -> Int -> DecodeState -> DecodeState
processState hashValue bits state =
    let
        bit : Int
        bit =
            (Bitwise.and (Bitwise.shiftRightBy bits hashValue) 1)
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


convertHashToBase32Code : String -> List Int
convertHashToBase32Code hashString =
    String.toList hashString
        |> List.filterMap convertCharToBase32Code


convertCharToBase32Code : Char -> Maybe Int
convertCharToBase32Code char =
    Dict.get (Char.toLower char) base32CodesDict
