module Pendulum exposing (Pendulum, renderPendulum, updatePendulum)

import Messages exposing (Msg(..))
import Point exposing (Point(..))
import Svg exposing (..)
import Svg.Attributes exposing (..)

type alias Pendulum =
    { pivotLocation : Point Int
    , theta : Float
    , thetadot : Float
    , thetadotdot : Float
    , len : Int
    , damping : Float
    , driving : Float
    , drivingFreq : Float
    }

calcThetadotdot : Pendulum -> Float -> Float
calcThetadotdot { theta, thetadot, thetadotdot
                , len, damping, driving, drivingFreq
                } time =
    let
        accelerationMass    = ((-9.8/100) / (toFloat len)) * sin theta
        accelerationDamping = damping * thetadot
        accelerationDriving = driving * 100.0 / (toFloat len) * cos (drivingFreq * time)
    in
        accelerationMass - accelerationDamping + accelerationDriving

updatePendulum : Pendulum -> Float -> Float -> Pendulum
updatePendulum pendulum time step =
    let
        thetadotdot =
            calcThetadotdot pendulum time

        thetadot =
            pendulum.thetadot + thetadotdot * step

        theta =
            pendulum.theta + thetadot * step

        thetaFix =
            if theta > pi then
                theta - 2 * pi
            else if theta <= -pi then
                2 * pi + theta
            else
                theta
    in
        { pendulum
          | theta = thetaFix
          , thetadot = thetadot
          , thetadotdot = thetadotdot
        }


renderPendulum : Pendulum -> List (Svg Msg)
renderPendulum {theta, len, pivotLocation} =
    let
        pendulumCoords = Point.add (Point.toInt (Point.fromPolarCoords (theta, toFloat len))) pivotLocation
        pCoordsLog = Debug.log (Point.logPointI pendulumCoords)
    in
        [ circle
            [ cx (String.fromInt (Point.getX pendulumCoords))
            , cy (String.fromInt (Point.getY pendulumCoords))
            , r "20"
            , stroke "black"
            , fill "black"
            ]
            []
        , line
            [ x1 (String.fromInt (Point.getX pivotLocation))
            , y1 (String.fromInt (Point.getY pivotLocation))
            , x2 (String.fromInt (Point.getX pendulumCoords))
            , y2 (String.fromInt (Point.getY pendulumCoords))
            , Svg.Attributes.style "stroke:rgb(0,0,0);stroke-width:2"
            ]
            []
        ]
