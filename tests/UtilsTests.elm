module UtilsTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, float)
import Test exposing (..)
import Utils exposing (..)

suite : Test
suite =
    describe "Tests for the Utility module"
        [ describe "Init tests"
              [ test "Testing with normal data" <|
                    \_ ->
                        [1,2,3,4,5]
                            |> init
                            |> Expect.equal [1,2,3,4]
              , test "With a single element list" <|
                    \_ ->
                        [1]
                            |> init
                            |> Expect.equal []
              , test "With an empty list" <|
                    \_ ->
                        []
                            |> init
                            |> Expect.equal []
              ]
        , describe "applyIndex tests"
              [ test "Testing with normal data" <|
                    \_ ->
                        [1,2,3,4,5]
                            |> applyIndex (\x -> x + 1) 2
                            |> Expect.equal [1,2,4,4,5]
              , test "Testing with edge index" <|
                    \_ ->
                        [1,2,3,4,5]
                            |> applyIndex (\x -> x + 1) 0
                            |> Expect.equal [2,2,3,4,5]
              , test "Testing with an index out of range" <|
                    \_ ->
                        [1,2,3,4,5]
                            |> applyIndex (\x -> x + 1) 100
                            |> Expect.equal [1,2,3,4,5]
              , test "Testing with an empty list" <|
                    \_ ->
                        []
                            |> applyIndex (\x -> x + 1) 0
                            |> Expect.equal []
              ]
        ]
