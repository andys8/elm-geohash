module Geohash exposing (encode, decodeCoordinate, decodeBoundingBox)

{-| This module is a Geohash encoding and decoding implementation in pure Elm.

Thanks to [Ning Sun](https://github.com/sunng87) for the [JavaScript implementation](https://github.com/sunng87/node-geohash).


# Functions

@docs encode, decodeCoordinate, decodeBoundingBox

-}

import GeohashDecode
import GeohashEncode


{-| Encodes coordinate (latitude, longitude, precision) to geohash.

    encode 57.648 10.41 6 == "u4pruy"

-}
encode : Float -> Float -> Int -> String
encode =
    GeohashEncode.encode


{-| Decodes geohash and returns center coordinate.

    decodeCoordinate "u281zk"
        == { lat = 48.14483642578125
           , lon = 11.5740966796875
           , latError = 0.00274658203125
           , lonError = 0.0054931640625
           }

-}
decodeCoordinate : String -> { lat : Float, lon : Float, latError : Float, lonError : Float }
decodeCoordinate =
    GeohashDecode.decode


{-| Decodes a geohash and returns the bounding box.

    decodeBoundingBox "u281zk" =
        { minLat = 48.14208984375
        , minLon = 11.568603515625
        , maxLat = 48.1475830078125
        , maxLon = 11.57958984375
        }

-}
decodeBoundingBox : String -> { minLat : Float, minLon : Float, maxLat : Float, maxLon : Float }
decodeBoundingBox =
    GeohashDecode.decodeBoundingBox
