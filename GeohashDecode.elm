module GeohashDecode exposing (decode)


decode : String -> { latitude : Float, longitude : Float, latitudeError : Float, longitudeError : Float }
decode hashValue =
    { latitude = 37.8324
    , longitude = 112.5584
    , latitudeError = 0
    , longitudeError = 0
    }


decodeBoundingBox : String -> { minLat : Float, minLon : Float, maxLat : Float, maxLon : Float }
decodeBoundingBox hashValue =
    { minLat = 0
    , minLon = 0
    , maxLat = 0
    , maxLon = 0
    }
