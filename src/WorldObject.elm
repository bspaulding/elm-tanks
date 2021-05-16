module WorldObject exposing (..)

import Angle exposing (..)
import Position exposing (..)


type WorldObject a
    = WorldObject Position Angle a


direction : WorldObject a -> Angle
direction (WorldObject p dir a) =
    dir


updateDirection : WorldObject a -> Angle -> WorldObject a
updateDirection (WorldObject p _ a) d =
    WorldObject p d a


moveObject : WorldObject a -> Angle -> Float -> Float -> WorldObject a
moveObject (WorldObject position objectAngle a) movementAngle timeDelta speed =
    let
        uCircleP =
            Angle.unitCirclePosition movementAngle

        speedFactor =
            timeDelta * speed

        newPosition =
            { x = position.x + speedFactor * uCircleP.x
            , y = position.y + speedFactor * uCircleP.y
            }
    in
    WorldObject newPosition objectAngle a


rotateAnticlockwise : WorldObject a -> Float -> Float -> WorldObject a
rotateAnticlockwise (WorldObject position angle a) timeDelta speed =
    let
        speedFactor =
            timeDelta * speed

        newAngle =
            toFloat (Angle.toDegrees angle)
                - speedFactor
                |> round
    in
    WorldObject position (Degrees newAngle) a


rotateClockwise : WorldObject a -> Float -> Float -> WorldObject a
rotateClockwise (WorldObject position angle a) timeDelta speed =
    let
        speedFactor =
            timeDelta * speed

        newAngle =
            toFloat (Angle.toDegrees angle)
                + speedFactor
                |> round
    in
    WorldObject position (Degrees newAngle) a
