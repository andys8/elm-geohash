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

import GeohashEncode


{-| Encodes latitude, longitude, precision to geohash.

    encode 57.648 10.41 6 == "u4pruy"
-}
encode : Float -> Float -> Int -> String
encode =
    GeohashEncode.encode
