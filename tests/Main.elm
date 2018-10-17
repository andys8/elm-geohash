module Main exposing (main)

import Browser
import Html exposing (br, div, h1, h3, text)
import Json.Encode exposing (Value)
import Random exposing (Generator)
import SimpleTest exposing (..)
import Tests


type alias Model =
    { test : Test }


initialModel : Model
initialModel =
    { test = Tests.all }


convertTestToString : Test -> List String
convertTestToString ts =
    case ts of
        UnitTest funcTest ->
            [ "doing UnitTest : " ]
                ++ List.map (\exp -> expectationToString exp) (funcTest ())

        FuzzTest funcFuzz ->
            [ "FuzzTest not implemented yet ... " ]

        Labeled str1 test2 ->
            [ "doing labeled string test : " ++ str1 ]
                ++ convertTestToString test2

        Skipped test2 ->
            [ "Skipped Test not implemented yet ... " ]

        Only test2 ->
            [ "Only Test not implemented yet ... " ]

        Batch ltests ->
            List.concatMap (\t -> convertTestToString t) ltests


countPassFail : List String -> ( Int, Int )
countPassFail ls =
    let
        nrTestsPassed =
            List.filter (\elem -> String.startsWith "test passed" elem) ls
                |> List.length

        nrTestsFailed =
            List.filter (\elem -> String.startsWith "test failed" elem) ls
                |> List.length
    in
    ( nrTestsPassed, nrTestsFailed )


view : Model -> Html.Html msg
view model =
    let
        lstrs =
            convertTestToString model.test

        ( nrTestsPassed, nrTestsFailed ) =
            countPassFail lstrs
    in
    div []
        [ h1 [] [ text ("number of Unit Tests passed : " ++ String.fromInt nrTestsPassed) ]
        , h1 [] [ text ("number of Unit Tests failed : " ++ String.fromInt nrTestsFailed) ]
        , br [] []
        , h3 [] (List.concatMap (\str -> [ text str, br [] [] ]) lstrs)
        ]


main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = \_ model -> model
        }



--port emit : ( String, Value ) -> Cmd msg
