module Pendulum exposing (..)

import Editor exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Messages exposing (Msg(..))
import Point exposing (Point(..))
import Svg exposing (..)
import Svg.Attributes as S exposing (..)

type alias Bob =
    { pivotLocation : Point Int
    , theta : Float
    , thetadot : Float
    , len : Int
    , damping : Float
    , driving : Float
    , drivingFreq : Float
    , mass :  Float
    }

type Pendulum = Single Bob
              | Double Bob Bob

initBob :  Bob
initBob =
    { pivotLocation = Point 300 220
    , theta = pi / 4
    , thetadot = 0
    , len = 150
    , damping = 0
    , driving = 0
    , drivingFreq = 0
    , mass = 1
    }

initPendulum : Pendulum
initPendulum =
    Single initBob

--                            Int here is index of which bob's thetadot we are calculating
--                            Also tells us which thetadot has been passed in and which is going to be partially applied
--                            If index is 0 then that thetadot will be passed in as a value
calcThetadotdot : Pendulum -> Maybe Int -> Float -> Float -> Float
calcThetadotdot p index time thetadot =
    case p of
        Single {theta, len, damping, driving, drivingFreq} ->
            let
                accelerationMass    = ((-9.81) / ((toFloat len) / 100)) * sin theta
                accelerationDamping = damping * thetadot
                accelerationDriving = (driving / ((toFloat len) / 100))* cos (drivingFreq * time)
            in
                accelerationMass - accelerationDamping + accelerationDriving

        Double b1 b2 ->
            case index of
                Just 1 -> -- b1.thetadot is passed in as thetadot
                    let
                        b1len = (toFloat b1.len) / 100
                        b2len = (toFloat b2.len) / 100
                        g = 9.81
                        n1 = -g*(2 * b1.mass + b2.mass) * sin b1.theta
                        n2 = -g * b2.mass * sin (b1.theta - 2 * b2.theta)
                        n3 = -2 * b2.mass * b2.thetadot * b2.thetadot * b2len * sin (b1.theta - b2.theta)
                        n4 = -2 * sin(b1.theta - b2.theta)* thetadot * thetadot * b1len * cos(b1.theta - b2.theta)
                        d = b1len * (2 * b1.mass + b2.mass - b2.mass * cos (2 * (b1.theta - b2.theta)))
                    in
                        (n1+n2+n3+n4)/d

                Just 2 -> -- b2.thetadot is passed in as thetadot
                    let
                        b1len = (toFloat b1.len) / 100
                        b2len = (toFloat b2.len) / 100
                        g = 9.81
                        n1 = 2 * sin (b1.theta - b2.theta)
                        n2 = (b1.mass + b2.mass) * b1.thetadot * b1.thetadot * b1len
                        n3 = g * (b1.mass + b2.mass) * cos b1.theta
                        n4 = b2.mass * thetadot * thetadot * b2len * cos (b1.theta - b2.theta)
                        d = b2len * (2 * b1.mass + b2.mass - b2.mass * cos (2 * (b1.theta - b2.theta)))
                    in
                        n1 * (n2 + n3 + n4) / d
                _ ->
                    thetadot

updatePendulum : Float -> Float -> Pendulum -> Pendulum
updatePendulum time step p =
    case p of
        Single bob ->
            let
                thetadot =
                    rungeKutta (calcThetadotdot p Nothing) (time, bob.thetadot) step

                theta =
                    rungeKutta (\_ _-> thetadot) (time, bob.theta) step

                newBob = { bob | theta = theta
                               , thetadot = thetadot
                         }
            in
                Single newBob

        Double b1 b2 ->
            let
                thetadot1 =
                    rungeKutta (calcThetadotdot p (Just 1)) (time, b1.thetadot) step

                theta1 =
                    rungeKutta (\_ _-> thetadot1) (time, b1.theta) step

                b1Location =
                    Point.add
                        (Point.toInt (Point.fromPolar (toFloat b1.len, theta1))) b1.pivotLocation

                thetadot2 =
                    rungeKutta (calcThetadotdot p (Just 2)) (time, b2.thetadot) step

                theta2 =
                    rungeKutta (\_ _-> thetadot2) (time, b2.theta) step

                newB1 = { b1 | theta = theta1
                             , thetadot = thetadot1
                        }

                newB2 = { b2 | theta = theta2
                             , thetadot = thetadot2
                             , pivotLocation = b1Location
                        }
            in
                Double newB1 newB2



order2 : (Float -> Float -> Float) -> (Float, Float) -> Float -> Float
order2 f (t, y) step =
    y + step * f (t + step / 2) (y + (f t y) * step / 2 )

-- takes in f(time, thetadot) = thetadotdot
-- takes in (time, thetadot) current
-- returns new thetadot
rungeKutta : (Float -> Float -> Float) -> (Float, Float) -> Float -> Float
rungeKutta f (t, y) step =
    let
        k1 = f t y
        k2 = f (t + step / 2) (y + step * k1 / 2)
        k3 = f (t + step / 2) (y + step * k2 / 2)
        k4 = f (t + step) (y + step * k3)
    in
        y + step * (k1 + 2 * k2 + 2 * k3 + k4) / 6

euler : (Float -> Float -> Float) -> (Float, Float) -> Float -> Float
euler f (t, y) step =
    y + step * f t y

toggleDouble : Pendulum -> Pendulum
toggleDouble p =
    case p of
        Single bob ->
            let
                bob1Location = Point.add (Point.toInt (Point.fromPolar (toFloat bob.len, bob.theta))) bob.pivotLocation

                b2 =
                    { pivotLocation = bob1Location
                    , theta = pi / 7
                    , thetadot = 0
                    , len = 150
                    , damping = 0
                    , driving = 0
                    , drivingFreq = 0
                    , mass = 1
                    }
            in
                Double bob b2

        Double b1 _ ->
            Single b1


renderBob :  Float -> Bob -> List (Svg Msg)
renderBob time ({theta, len, pivotLocation} as b) =
    let
        pendulumCoords = Point.add (Point.toInt (Point.fromPolar (toFloat len, theta))) pivotLocation
    in
        [ circle
            [ cx (String.fromInt (Point.getX pendulumCoords))
            , cy (String.fromInt (Point.getY pendulumCoords))
            , r "20"
            , stroke "black"
            , fill "black"
            ] []
        , line
            [ x1 (String.fromInt (Point.getX pivotLocation))
            , y1 (String.fromInt (Point.getY pivotLocation))
            , x2 (String.fromInt (Point.getX pendulumCoords))
            , y2 (String.fromInt (Point.getY pendulumCoords))
            , S.style "stroke:rgb(0,0,0);stroke-width:2"
            ] []
        ]

renderPendulum : Float -> Pendulum -> List (Svg Msg)
renderPendulum time p =
    case p of
        Single bob ->
            renderBob time bob

        Double b1 b2 ->
            (renderBob time b1) ++ (renderBob time b2)

drawArrow : Point Int -> Point Int -> List (Svg Msg)
drawArrow (Point x1 y1) (Point x2 y2) =
    let
        k =
            if x2 > x1 then
                1
            else
                -1
    in
        [ line
              [ S.x1 (String.fromInt x1)
              , S.y1 (String.fromInt y1)
              , S.x2 (String.fromInt x2)
              , S.y2 (String.fromInt y2)
              , S.style "stroke:rgb(0,0,0);stroke-width:2"
              ] []
        , line
              [ S.x1 (String.fromInt (x2 - k * 20))
              , S.y1 (String.fromInt (y2 + 20))
              , S.x2 (String.fromInt x2)
              , S.y2 (String.fromInt y2)
              , S.style "stroke:rgb(0,0,0);stroke-width:2"
              ] []
        , line
              [ S.x1 (String.fromInt (x2 - k * 20))
              , S.y1 (String.fromInt (y2 - 20))
              , S.x2 (String.fromInt x2)
              , S.y2 (String.fromInt y2)
              , S.style "stroke:rgb(0,0,0);stroke-width:2"
              ] []
        ]

editorPendulum : Pendulum -> List (Editor)
editorPendulum p =
    case p of
        Single bob ->
            [[ { name = "length"
               , val = String.fromInt bob.len
               , min = "50"
               , max = "200"
               , step = "1"
               }
             , { name = "damping"
               , val = String.fromFloat bob.damping
               , min = "0.0"
               , max = "0.5"
               , step = "0.01"
               }
             , { name = "drive force"
               , val = String.fromFloat bob.driving
               , min = "0.0"
               , max = "50.0"
               , step = "0.1"
              }
             , { name ="drive freq"
               , val = String.fromFloat bob.drivingFreq
               , min = "0.0"
               , max = "1.0"
               , step = "0.01"
              }
             ]]

        Double b1 b2 ->
            [[ { name = "length 1"
               , val = String.fromInt b1.len
               , min = "50"
               , max = "200"
               , step = "1"
               }
             , { name = "mass 1"
               , val = String.fromFloat b1.mass
               , min = "0.1"
               , max = "10"
               , step = "0.1"
               }
             , { name = "length 2"
               , val = String.fromInt b2.len
               , min = "50"
               , max = "200"
               , step = "1"
               }
             , { name = "mass 2"
               , val = String.fromFloat b2.mass
               , min = "0.1"
               , max = "10"
               , step = "0.1"
               }
             ]]

updateVal : String -> Maybe Int -> Pendulum -> Float -> (Maybe Pendulum)
updateVal name index p val =
    case p of
        Single bob ->
            Maybe.map
                (\b -> Single b)
                (updateBob name bob val)

        Double b1 b2 ->
            case index of
                Just 1 ->
                    Maybe.map
                        (\b -> Double b b2)
                        (updateBob name b1 val)
                Just 2 ->
                    Maybe.map
                        (\b -> Double b1 b)
                        (updateBob name b2 val)
                _ ->
                    Nothing

updateBob : String -> Bob -> Float -> Maybe Bob
updateBob name bob val =
    if String.contains "length" name then
        Just { bob | len = (round val)}
    else if String.contains "mass" name then
        Just { bob | mass = val}
    else
        case name of
            "damping" ->
                Just { bob | damping = val }
            "drive force" ->
                Just { bob | driving = val }
            "drive freq" ->
                Just { bob | drivingFreq = val }
            "mass" ->
                Just { bob | mass = val }
            _ ->
                Nothing
