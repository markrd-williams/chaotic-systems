module Model exposing (..)

type Point number  = Point number number

fromCoords : ( number, number ) -> Point number
fromCoords (x, y) = Point x y 

fromPolarCoords : ( Float, Float ) -> Point Float 
fromPolarCoords polar = fromCoords (fromPolar polar)

type alias Pendulum =
    { pivotLocation : Point Int
    , theta : Float
    , thetadot : Float
    , thetadotdot : Float
    , len : Int
    , damping : Float
    , driving : Float
    , drivingFrequency : Float
    }

type alias Model =
    { pageNumber : Int
    , pendulum : Pendulum
    }

initModel : Model
initModel =
    Model 1
