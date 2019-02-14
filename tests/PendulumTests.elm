module PendulumTests exposing (..)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, float)
import Pendulum exposing (..)
import Test exposing (..)
import Seq exposing (..)
import Point exposing (Point(..))

createDiffEqTests approximator name =
    let
        realData = List.map (\x -> e ^ (toFloat x / 10)) (List.range 0 10)
        deApprox =
            (\y -> approximator (\b x -> x) (0, y) 0.1)
        approxData = toList (take 11 (iterate deApprox 1))
        createTest a b =
            test (name ++ " " ++ String.fromFloat a) <|
                \_ ->
                    a |> Expect.within (Absolute 0.0001) b
    in
        List.map2 createTest approxData realData

suite : Test
suite =
    describe "Pendulum Tests"
        [ describe "Numerical integration tests"
              [ describe "Runge Kutta 4 tests" (createDiffEqTests rungeKutta "rungeKutta")
 --             , describe "Euler tests"  (createDiffEqTests euler "euler")
              ]
        , describe "calcThetadotdot tests"
              [ test "Normal data test" <|
                    \_ ->
                        calcThetadotdot initPendulum Nothing 0 0
                            |> Expect.within (Absolute 0.0001) -4.624478
              , test "Damping test" <|
                    \_ ->
                        let
                            dampedP = Single {initBob | damping = 0.5}
                        in
                            calcThetadotdot dampedP Nothing 0 -4
                                |> Expect.within (Absolute 0.0001) -2.6244783
              , test "Driving test" <|
                    \_ ->
                        let
                            drivenP = Single {initBob | driving = 0.5
                                                      , drivingFreq = pi
                                             }
                        in
                            calcThetadotdot drivenP Nothing 0 0
                                |> Expect.within (Absolute 0.0001) -4.29114501
              , test "Combined test" <|
                    \_ ->
                        let
                            pend = Single {initBob | driving = 0.5
                                                   , drivingFreq = pi
                                                   , damping = 0.5
                                          }
                        in
                            calcThetadotdot pend Nothing 0 -4
                                |> Expect.within (Absolute 0.0001) -2.291145016
              , test "Double initial pend1 test" <|
                    \_ ->
                        let
                            doubleP = Double initBob initBob
                        in
                            calcThetadotdot doubleP (Just 1) 0 0
                                |> Expect.within (Absolute 0.0001) -4.624478349
              , test "Double pend1 test" <|
                    \_ ->
                        let
                            doubleP = Double {initBob | thetadot = 0.2, theta = pi/2} {initBob | theta = pi/7, thetadot = -2.5, pivotLocation = Point.add (Point.toInt (Point.fromPolar (150, pi/2))) (Point 300 220)}
                        in
                            calcThetadotdot doubleP (Just 1) 0 0.2
                                |> Expect.within (Absolute 0.0001) -9.65671

              , test "Double pend2 test" <|
                    \_ ->
                        let
                            doubleP = Double {initBob | thetadot = 0.2, theta = pi/2} {initBob | theta = pi/7, thetadot = -2.5, pivotLocation = Point.add (Point.toInt (Point.fromPolar (150, pi/2))) (Point 300 220)}
                        in
                            calcThetadotdot doubleP (Just 2) 0 -2.5
                                |> Expect.within (Absolute 0.0001) 1.38833

              , test "Double initial pend2 test" <|
                    \_ ->
                        let
                            doubleP = Double initBob initBob
                        in
                            calcThetadotdot doubleP (Just 2) 0 0
                                |> Expect.within (Absolute 0.0001) 0
              ]
        ]
