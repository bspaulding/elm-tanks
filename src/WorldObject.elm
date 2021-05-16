module WorldObject exposing (..)

import Angle exposing (..)
import Position exposing (..)
import Size exposing (..)
import World


type WorldObject a
    = WorldObject Position Angle Size a


get : (a -> b) -> WorldObject a -> b
get f (WorldObject p dir s a) =
    f a


updateObject : WorldObject a -> (a -> a) -> WorldObject a
updateObject (WorldObject p dir s a) f =
    WorldObject p dir s (f a)


position : WorldObject a -> Position
position (WorldObject p dir size a) =
    p


direction : WorldObject a -> Angle
direction (WorldObject p dir size a) =
    dir


updateDirection : WorldObject a -> Angle -> WorldObject a
updateDirection (WorldObject p _ s a) d =
    WorldObject p d s a


moveObject : WorldObject a -> Angle -> Float -> Float -> WorldObject a
moveObject (WorldObject p objectAngle size a) movementAngle timeDelta speed =
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
    WorldObject newPosition objectAngle size a


rotateAnticlockwise : WorldObject a -> Float -> Float -> WorldObject a
rotateAnticlockwise (WorldObject p angle size a) timeDelta speed =
    let
        speedFactor =
            timeDelta * speed

        newAngle =
            toFloat (Angle.toDegrees angle)
                - speedFactor
                |> round
    in
    WorldObject p (Degrees newAngle) size a


rotateClockwise : WorldObject a -> Float -> Float -> WorldObject a
rotateClockwise (WorldObject p angle size a) timeDelta speed =
    let
        speedFactor =
            timeDelta * speed

        newAngle =
            toFloat (Angle.toDegrees angle)
                + speedFactor
                |> round
    in
    WorldObject p (Degrees newAngle) size a


inBounds : WorldObject a -> Bool
inBounds (WorldObject p dir size a) =
    p.x > 0 && p.x < World.width && p.y > 0 && p.y < World.height


isColliding : WorldObject a -> WorldObject b -> Bool
isColliding (WorldObject pa _ sa _) (WorldObject pb _ sb _) =
    isOverlapping (box pa sa) (box pb sb)


type alias Box =
    { x : Float, width : Float, y : Float, height : Float }


xmin : Box -> Float
xmin b =
    b.x


xmax : Box -> Float
xmax b =
    b.x + b.width


ymin : Box -> Float
ymin b =
    b.y


ymax : Box -> Float
ymax b =
    b.y + b.height


box : Position -> Size -> Box
box p s =
    { x = p.x - s.width / 2
    , width = s.width
    , y = p.y - s.height / 2
    , height = s.height
    }


isOverlapping : Box -> Box -> Bool
isOverlapping b1 b2 =
    isOverlapping1D ( xmin b1, xmax b1 ) ( xmin b2, xmax b2 )
        && isOverlapping1D ( ymin b1, ymax b1 ) ( ymin b2, ymax b2 )


isOverlapping1D : ( Float, Float ) -> ( Float, Float ) -> Bool
isOverlapping1D ( xmin1, xmax1 ) ( xmin2, xmax2 ) =
    xmax1 >= xmin2 && xmax2 >= xmin1
