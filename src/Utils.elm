module Utils exposing (..)

init : List a -> List a
init l =
    case l of
        [] -> []
        [x] -> []
        (x::xs) -> x :: init xs
