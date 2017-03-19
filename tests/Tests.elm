module Tests exposing (all)

import Test exposing (..)
import Expect
import Geohash exposing (encode, decode, decodeBoundingBox)


all : Test
all =
    describe "Geohash"
        [ describe "Geohash.encode"
            [ test "encodes Jutland" <|
                \() ->
                    Geohash.encode 57.648 10.41 6
                        |> Expect.equal "u4pruy"
            , test "encodes Curitiba" <|
                \() ->
                    Geohash.encode -25.38262 -49.26561 8
                        |> Expect.equal "6gkzwgjz"
            , test "precision 3" <|
                \() ->
                    Geohash.encode 32 117 3
                        |> Expect.equal "wte"
            , test "precision 9" <|
                \() ->
                    Geohash.encode 37.8324 112.5584 9
                        |> Expect.equal "ww8p1r4t8"
            , test "high accuracy" <|
                \() ->
                    Geohash.encode 48.14319769313948 11.536403596401215 12
                        |> Expect.equal "u281ys0w3wtb"
            ]
        , describe "Geohash.decode"
            [ test "decoded latitude" <|
                \() ->
                    Geohash.decode "ww8p1r4t8"
                        |> .latitude
                        |> (-) 37.8324
                        |> abs
                        |> Expect.lessThan 0.0001
            , test "decoded longitude" <|
                \() ->
                    Geohash.decode "ww8p1r4t8"
                        |> .longitude
                        |> (-) 112.5584
                        |> abs
                        |> Expect.lessThan 0.0001
            , test "decode t longitude" <|
                \() ->
                    Geohash.decode "t"
                        |> .longitude
                        |> (-) (45 + (90 - 45) / 2)
                        |> abs
                        |> Expect.lessThan 0.0001
            , test "decode t " <|
                \() ->
                    Geohash.decode "t"
                        |> .latitude
                        |> (-) (45 / 2)
                        |> abs
                        |> Expect.lessThan 0.0001
            ]
        , describe "Geohash.decodeBoundingBox"
            [ test "decode t" <|
                \() ->
                    Geohash.decodeBoundingBox "t"
                        |> Expect.equal { minLat = 0, minLon = 45, maxLat = 45, maxLon = 90 }
            , test "decode munich" <|
                \() ->
                    Geohash.decodeBoundingBox "u281"
                        |> Expect.equal { minLat = 47.98828125, minLon = 11.25, maxLat = 48.1640625, maxLon = 11.6015625 }
            ]
        ]
