module Tests exposing (all)

import Test exposing (..)
import Expect
import Geohash exposing (encode, decode)


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
                        |> Expect.equal 37.8324
            , test "decoded longitude" <|
                \() ->
                    Geohash.decode "ww8p1r4t8"
                        |> .longitude
                        |> Expect.equal 112.5584
            ]
        ]



{--

    it('encodes Jutland',     function() { Geohash.encode(57.648, 10.410, 6).should.equal('u4pruy'); });
    it('decodes Jutland',     function() { Geohash.decode('u4pruy').should.deep.equal({ lat: 57.648, lon: 10.410 }); });
    it('encodes Curitiba',    function() { Geohash.encode(-25.38262, -49.26561, 8).should.equal('6gkzwgjz'); });
    it('decodes Curitiba',    function() { Geohash.decode('6gkzwgjz').should.deep.equal({ lat: -25.38262, lon: -49.26561 }); });
    it('fetches neighbours',  function() { Geohash.neighbours('ezzz').should.deep.equal({ n:'gbpb', ne:'u000', e:'spbp', se:'spbn', s:'ezzy', sw:'ezzw', w:'ezzx', nw:'gbp8' }); });
    it('matches geohash.org', function() { Geohash.encode(37.25, 123.75, 12).should.equal('wy85bj0hbp21'); }); // (also PostGIS; thx Jussi Nieminen)

--}
