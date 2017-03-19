module Tests exposing (all)

import Test exposing (..)
import Expect
import Geohash exposing (encode)


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
            ]
        ]
