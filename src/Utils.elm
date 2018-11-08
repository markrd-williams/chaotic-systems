module Utils exposing (..)

init : List a -> List a
init l =
    case l of
        [] -> []
        [x] -> []
        (x::xs) -> x :: init xs

applyIndex : (a -> a) -> Int -> List a -> List a
applyIndex f index = List.indexedMap (\i item -> if index == i then f item else item)
