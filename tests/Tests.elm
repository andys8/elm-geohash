module Tests exposing (all)

import Geohash exposing (decodeBoundingBox, decodeCoordinate, encode)
import SimpleTest exposing (..)


equalityThreshold : Float
equalityThreshold =
    0.0001


all : Test
all =
    describe "Geohash"
        [ describe "encode"
            [ test "Jutland" <|
                \() ->
                    encode 57.648 10.41 6
                        |> expectEqual "u4pruy"
            , test "Curitiba" <|
                \() ->
                    encode -25.38262 -49.26561 8
                        |> expectEqual "6gkzwgjz"
            , test "with precision 3" <|
                \() ->
                    encode 32 117 3
                        |> expectEqual "wte"
            , test "with precision 9" <|
                \() ->
                    encode 37.8324 112.5584 9
                        |> expectEqual "ww8p1r4t8"
            , test "with precision 12" <|
                \() ->
                    encode 48.14319769313948 11.536403596401215 12
                        |> expectEqual "u281ys0w3wtb"
            ]
        , describe "decodeCoordinate"
            [ test "decode latitude of ww8p1r4t8" <|
                \() ->
                    decodeCoordinate "ww8p1r4t8"
                        |> .lat
                        |> (-) 37.8324
                        |> abs
                        |> expectLessThan equalityThreshold
            , test "decode longitude of ww8p1r4t8" <|
                \() ->
                    decodeCoordinate "ww8p1r4t8"
                        |> .lon
                        |> (-) 112.5584
                        |> abs
                        |> expectLessThan equalityThreshold
            , test "decode longitude of t" <|
                \() ->
                    decodeCoordinate "t"
                        |> .lon
                        |> (-) (45 + (90 - 45) / 2)
                        |> abs
                        |> expectLessThan equalityThreshold
            , test "decode latitude of t " <|
                \() ->
                    decodeCoordinate "t"
                        |> .lat
                        |> (-) (45 / 2)
                        |> abs
                        |> expectLessThan equalityThreshold
            ]
        , describe "decodeBoundingBox"
            [ test "t" <|
                \() ->
                    decodeBoundingBox "t"
                        |> expectEqual { minLat = 0, minLon = 45, maxLat = 45, maxLon = 90 }
            , test "munich" <|
                \() ->
                    decodeBoundingBox "u281"
                        |> expectEqual { minLat = 47.98828125, minLon = 11.25, maxLat = 48.1640625, maxLon = 11.6015625 }
            ]
        ]
