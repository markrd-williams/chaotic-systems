module Point exposing (..)

type Point number  = Point number number

fromCoords : ( number, number ) -> Point number
fromCoords (x, y) = Point x y 

fromPolarCoords : ( Float, Float ) -> Point Float 
fromPolarCoords polar = fromCoords (fromPolar polar)


