module Pendulum exposing (Pendulum)

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
        accelerationMass = ((-9.8/100) / len) * sin theta
        accelerationDamping = damping * thetadot
        accelerationDamping = driving * 100 / len * cos (drivingFreq * time)

rungeKutta : (Float -> Float -> Float) 
           -> Float -> Point number 
           -> Point number
rungeKutta f h (Point x y) =
    let
        k1 = h * (f x y)
        k2 = h * (f (x + h/2) (y + k1/2))
        k3 = h * (f (x + h/2) (y + k2/2))
        k4 = h * (f (x + h) (y + k3))
    in
        Point (x + h) 
              (y + (k1 + 2 * k2 + 2 * k3 + k4)/6)



renderPendulum : Pendulum -> List (Svg Msg)
renderPendulum {theta, len, pivotLocation} =
    let
        coords = (Point.fromPolarCoords (theta, len)) + pivotLocation
    in
        [ circle 
            [ cx (toString (x coords))
            , cy (toString (y coords))
            , r "20"
            , stroke "black"
            , fill "black"
            ]
        , line 
            [ 
            ]
        ]


        


        
