module PointTests exposing (..)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, float)
import Point exposing (Point(..))
import Test exposing (..)

suite : Test
suite =
    describe "The Point module"
        [ describe "Point.fromCoords"
              [ test "Integer tuples are converted to Points" <|
                    \_ ->
                        (1,2)
                            |> Point.fromCoords
                            |> Expect.equal (Point 1 2)
              , test "Float tuples are converted to Points" <|
                    \_ ->
                        (1.0, 2.0)
                            |> Point.fromCoords
                            |> Expect.equal (Point 1.0 2.0)
              ]
        , describe "Point.fromPolar and Point.toPolarCoords"
              [ test "Tuples return the correct cartesian coordinate" <|
                    \_ ->
                        (1, 0)
                            |> Point.fromPolar
                            |> Expect.equal (Point 0 1)
              , test "Points can be converted to polar coordinate tuples" <|
                    \_ ->
                        Point 1 0
                            |> Point.toPolarCoords
                            |> Expect.equal (1, 0)
              ]
        , describe "Point.add"
              [ test "Add integer points" <|
                    \_ ->
                        Point 1 5
                            |> Point.add (Point 2 3)
                            |> Expect.equal (Point 3 8)
              , test "Add float points" <|
                    \_ ->
                        Point 1.3 2.1
                            |> Point.add (Point 1.2 3.3)
                            |> Expect.equal (Point 2.5 5.4)
              ]
        , describe "Point.getX"
              [ test "get x values correctly" <|
                    \_ ->
                        Point 4 2
                            |> Point.getX
                            |> Expect.equal 4
              ]
        , describe "Point.getY"
              [ test "get y values correctly" <|
                    \_ ->
                        Point 4 2
                            |> Point.getY
                            |> Expect.equal 2
              ]
        , describe "Point.toInt"
              [ test "turns a Point float into a Point int" <|
                    \_ ->
                        Point 4.5 9.9
                            |> Point.toInt
                            |> Expect.equal (Point 5 10)
              ]
        ]
