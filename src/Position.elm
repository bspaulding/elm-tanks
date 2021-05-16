module Position exposing (..)


type alias Position =
    { x : Float, y : Float }


add : Position -> Position -> Position
add p1 p2 =
    { x = p1.x + p2.x, y = p1.y + p2.y }
