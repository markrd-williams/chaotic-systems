module Point exposing (..)

type Point number  = Point number number

fromCoords : ( number, number ) -> Point number
fromCoords (x, y) = Point x y

fromPolarCoords : ( Float, Float ) -> Point Float
fromPolarCoords polar = fromCoords (fromPolar polar)

toPolarCoords : Point Float -> (Float, Float)
toPolarCoords (Point x y) = toPolar (x, y)

add : Point number -> Point number -> Point number
add (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

getX : Point number -> number
getX (Point x _) = x

getY : Point number -> number
getY (Point _ y) = y

toInt : Point Float -> Point Int
toInt (Point x y) = Point (round x) (round y)

logPointI : Point Int -> String
logPointI (Point x y) = "Point " ++ (String.fromInt x) ++ " " ++ (String.fromInt y)

logPointF : Point Float -> String
logPointF (Point x y) = "Point " ++ (String.fromFloat x) ++ " " ++ (String.fromFloat y)
