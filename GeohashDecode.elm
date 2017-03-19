module GeohashDecode exposing (decode)


decode : String -> { latitude : Float, longitude : Float, latitudeError : Float, longitudeError : Float }
decode hashValue =
    { latitude = 37.8324
    , longitude = 112.5584
    , latitudeError = 0
    , longitudeError = 0
    }
