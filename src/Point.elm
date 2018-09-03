module Point exposing (..)

type Point number  = Point number number

fromCoords : ( number, number ) -> Point number
fromCoords (x, y) = Point x y 

fromPolar : ( Float, Float ) -> Point Float 
fromPolar polar = fromCoords (fromPolar polar)

toPolar : Point number -> (number, number)
toPolar (Point x y) = toPolar (x, y)

(+) : Point number -> Point number -> Point number
(+) (Point x1 y1) (Point x2 y2) = Point (x1 + y1) (x2 + y2)

x : Point number -> number
x (Point x _) = x

y : Point number -> number
y (Point _ y) = y
