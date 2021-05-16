module Angle exposing (..)

import Position exposing (Position)


type Angle
    = Degrees Int


toRadians : Angle -> Float
toRadians (Degrees d) =
    toFloat d * (pi / 180)


toDegrees : Angle -> Int
toDegrees (Degrees d) =
    d


oppositeDirection : Angle -> Angle
oppositeDirection (Degrees d) =
    Degrees (d + 180)


unitCirclePosition : Angle -> Position
unitCirclePosition d =
    { x = cos (toRadians d), y = sin (toRadians d) }
