module SimpleTest exposing (Expectation(..), InvalidReason(..), Reason(..), Test(..), blankDescriptionFailure, compareWith, describe, duplicatedName, equateWith, expectEqual, expectFail, expectLessThan, expectationToString, fail, failNow, internalToString, pass, test, testWith)

import Random
import Set exposing (Set)



-- cant seem to make elm-explorations/test  run so i just copied the relevant code ( given the simple tests we want to run ) to this file
-- https://github.com/elm-explorations/test


type Reason
    = Custom
    | Equality String String
    | Comparison String String
    | ListDiff (List String) (List String)
    | CollectionDiff
        { expected : String
        , actual : String
        , extra : List String
        , missing : List String
        }
    | TODO
    | Invalid InvalidReason


{-| The reason a test run was invalid.
Test runners should report these to the user in whatever format is appropriate.
-}
type InvalidReason
    = EmptyList
    | NonpositiveFuzzCount
    | InvalidFuzzer
    | BadDescription
    | DuplicatedName


type Expectation
    = Pass
    | Fail { given : Maybe String, description : String, reason : Reason }


expectationToString : Expectation -> String
expectationToString expectation =
    case expectation of
        Pass ->
            "test passed"

        Fail rec ->
            "test failed with given : " ++ Maybe.withDefault "" rec.given ++ " , and description : " ++ rec.description


type Test
    = UnitTest (() -> List Expectation)
    | FuzzTest (Random.Seed -> Int -> List Expectation)
    | Labeled String Test
    | Skipped Test
    | Only Test
    | Batch (List Test)


test : String -> (() -> Expectation) -> Test
test untrimmedDesc thunk =
    let
        desc =
            String.trim untrimmedDesc
    in
    if String.isEmpty desc then
        blankDescriptionFailure

    else
        Labeled desc (UnitTest (\() -> [ thunk () ]))


expectEqual : a -> a -> Expectation
expectEqual =
    equateWith "Expect.equal" (==)


expectLessThan : comparable -> comparable -> Expectation
expectLessThan =
    compareWith "Expect.lessThan" (<)


{-| Create a failure without specifying the given.
-}
fail : { description : String, reason : Reason } -> Expectation
fail { description, reason } =
    Fail { given = Nothing, description = description, reason = reason }


{-| Create a test that always fails for the given reason and description.
-}
failNow : { description : String, reason : Reason } -> Test
failNow record =
    UnitTest
        (\() -> [ fail record ])


blankDescriptionFailure : Test
blankDescriptionFailure =
    failNow
        { description = "This test has a blank description. Let's give it a useful one!"
        , reason = Invalid BadDescription
        }


duplicatedName : List Test -> Result String (Set String)
duplicatedName =
    let
        names : Test -> List String
        names tst =
            case tst of
                Labeled str _ ->
                    [ str ]

                Batch subtests ->
                    List.concatMap names subtests

                UnitTest _ ->
                    []

                FuzzTest _ ->
                    []

                Skipped subTest ->
                    names subTest

                Only subTest ->
                    names subTest

        insertOrFail : String -> Result String (Set String) -> Result String (Set String)
        insertOrFail newName =
            Result.andThen
                (\oldNames ->
                    if Set.member newName oldNames then
                        Err newName

                    else
                        Ok <| Set.insert newName oldNames
                )
    in
    List.concatMap names
        >> List.foldl insertOrFail (Ok Set.empty)


describe : String -> List Test -> Test
describe untrimmedDesc tests =
    let
        desc =
            String.trim untrimmedDesc
    in
    if String.isEmpty desc then
        failNow
            { description = "This `describe` has a blank description. Let's give it a useful one!"
            , reason = Invalid BadDescription
            }

    else if List.isEmpty tests then
        failNow
            { description = "This `describe " ++ desc ++ "` has no tests in it. Let's give it some!"
            , reason = Invalid EmptyList
            }

    else
        case duplicatedName tests of
            Err duped ->
                failNow
                    { description = "The tests '" ++ desc ++ "' contain multiple tests named '" ++ duped ++ "'. Let's rename them so we know which is which."
                    , reason = Invalid DuplicatedName
                    }

            Ok childrenNames ->
                if Set.member desc childrenNames then
                    failNow
                        { description = "The test '" ++ desc ++ "' contains a child test of the same name. Let's rename them so we know which is which."
                        , reason = Invalid DuplicatedName
                        }

                else
                    Labeled desc (Batch tests)


internalToString : a -> String
internalToString =
    Elm.Kernel.Debug.toString


pass : Expectation
pass =
    Pass


expectFail : String -> Expectation
expectFail str =
    fail { description = str, reason = Custom }


equateWith : String -> (a -> b -> Bool) -> b -> a -> Expectation
equateWith reason comparison b a =
    let
        isJust x =
            case x of
                Just _ ->
                    True

                Nothing ->
                    False

        isFloat x =
            isJust (String.toFloat x) && not (isJust (String.toInt x))

        usesFloats =
            isFloat (internalToString a) || isFloat (internalToString b)

        floatError =
            if String.contains reason "not" then
                "Do not use Expect.notEqual with floats. Use Float.notWithin instead."

            else
                "Do not use Expect.equal with floats. Use Float.within instead."
    in
    if usesFloats then
        expectFail floatError

    else
        testWith Equality reason comparison b a


compareWith : String -> (a -> b -> Bool) -> b -> a -> Expectation
compareWith =
    testWith Comparison


testWith : (String -> String -> Reason) -> String -> (a -> b -> Bool) -> b -> a -> Expectation
testWith makeReason label runTest expected actual =
    if runTest actual expected then
        pass

    else
        { description = label
        , reason = makeReason (internalToString expected) (internalToString actual)
        }
            |> fail
