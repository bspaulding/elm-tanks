module WorldObject exposing (..)

import Angle exposing (..)
import Position exposing (..)
import World


type WorldObject a
    = WorldObject Position Angle a


position : WorldObject a -> Position
position (WorldObject p dir a) =
    p


direction : WorldObject a -> Angle
direction (WorldObject p dir a) =
    dir


updateDirection : WorldObject a -> Angle -> WorldObject a
updateDirection (WorldObject p _ a) d =
    WorldObject p d a


moveObject : WorldObject a -> Angle -> Float -> Float -> WorldObject a
moveObject (WorldObject p objectAngle a) movementAngle timeDelta speed =
    let
        uCircleP =
            Angle.unitCirclePosition movementAngle

        speedFactor =
            timeDelta * speed

        newPosition =
            { x = max 0 (min World.width (p.x + speedFactor * uCircleP.x))
            , y = max 0 (min World.height (p.y + speedFactor * uCircleP.y))
            }
    in
    WorldObject newPosition objectAngle a


rotateAnticlockwise : WorldObject a -> Float -> Float -> WorldObject a
rotateAnticlockwise (WorldObject p angle a) timeDelta speed =
    let
        speedFactor =
            timeDelta * speed

        newAngle =
            toFloat (Angle.toDegrees angle)
                - speedFactor
                |> round
    in
    WorldObject p (Degrees newAngle) a


rotateClockwise : WorldObject a -> Float -> Float -> WorldObject a
rotateClockwise (WorldObject p angle a) timeDelta speed =
    let
        speedFactor =
            timeDelta * speed

        newAngle =
            toFloat (Angle.toDegrees angle)
                + speedFactor
                |> round
    in
    WorldObject p (Degrees newAngle) a


inBounds : WorldObject a -> Bool
inBounds (WorldObject p dir a) =
    p.x > 0 && p.x < World.width && p.y > 0 && p.y < World.height


isColliding : WorldObject a -> WorldObject b -> Bool
isColliding (WorldObject pa _ _) (WorldObject pb _ _) =
    -- WorldObject needs a size
    False
